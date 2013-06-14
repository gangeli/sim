package org.goobs.sim

//(scala)
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import edu.stanford.nlp._

//(java)
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.util.zip.GZIPOutputStream
//(jaws)
import edu.smu.tspell.wordnet._
//(lib)
import edu.stanford.nlp.util.logging.Redwood.Util._
//(project)
import Util._

//------------------------------------------------------------------------------
// ONTOLOGY IMPLEMENTATION
//------------------------------------------------------------------------------

@SerialVersionUID(4l)
case class Ontology(ontology:Map[Phrase, Set[Ontology.RealNode]],
                    root:Ontology.AbstractNode,
                    posRoots:Set[Ontology.Node]) {
  import Ontology._

  def totalCount:Long = root.subtreeCount

  def depth:Int = 22 // computeDepth(root)
  def computeDepth(root:Node):Int = {
    if (root.hyponyms.isEmpty) 1
    else root.hyponyms.map{ computeDepth(_) }.max + 1
  }
    
  //-------------------------
  // I / O
  //-------------------------

  /**
   * Save a serializable version of this ontology to a file. Note that the
   * ontology should be re-read with Ontology.load() and not directly from an
   * ObjectInputStream
   * @param filename The file to save the ontology to.
   */
  def save(filename:String):Unit = {
    startTrack("Serializing ontology")
    // (removing hyponyms -- circular when serializing)
    this.foreach{ _.hyponyms.clear }
    root.hyponyms.clear
    posRoots.foreach( _.hyponyms.clear )
    log("removed circular links")
    // (writing file)
    val outStream = new ObjectOutputStream(
                        new GZIPOutputStream(new FileOutputStream(filename)))
    log("writing file " + filename)
    outStream.writeObject(this)
    outStream.close
    endTrack("Serializing ontology")
  }

  //-------------------------
  // Accessing Ontology
  //-------------------------

  /**
   *  Check if a phrase is in the ontology.
   *  @param phrase The string to look for.
   *  @return True if the ontology contains the phrase.
   */
  def contains(phrase:Seq[String]):Boolean = {
    val phraseAsList:List[String] = phrase.toList
    if (ontology.contains(phraseAsList)) true
    else if (ontology.contains(phraseAsList.map{ _.toLowerCase })) true
    else false
  }
  /** @see contains */
  def contains(phrase:String):Boolean = contains(phrase.split("""\s+"""))
  
  /**
   *  Get an element in the ontology, as a set of nodes.
   *  @param phrase The string to look for.
   *  @return A set of nodes corresponding to this phrase.
   */
  def get(phrase:Seq[String]):Set[RealNode] = {
    ontology.get(phrase.toList) match {
      case Some(node) => node
      case None =>
        ontology.get(phrase.toList.map{ _.toLowerCase }) match {
          case Some(node) => node
          case None => Set[RealNode]()
        }
    }
  }
  /** @see get */
  def get(phrase:String):Set[RealNode] = get(phrase.split("""\s+"""))

  /**
   *  Get the most common element in the ontology.
   *  @param phrase The string to look for.
   *  @return The most common node in the ontology, by its word count
   *          (_not_ subtree count)
   */
  def apply(phrase:Seq[String]):RealNode = get(phrase).maxBy( _.count )
  /** @see apply */
  def apply(phrase:String):RealNode = get(phrase).maxBy( _.count )

  /**
   *  Construct an object encapsulating the similarity between two phrases.
   *  @param a The first set of nodes to compare.
   *  @param b The second set of nodes to compare.
   *  @return An object which stores the similarity between the two phrases.
   */
  def sim(a:Set[_<:Node], b:Set[_<:Node]):Similarity
    = Similarity(a.asInstanceOf[Set[Node]], b.asInstanceOf[Set[Node]], totalCount)
 
  /** @see sim */
  def sim(a:Node, b:Node):Similarity = sim(Set[Node](a), Set[Node](b))

  /** Returns the similarity between two phrases.
    * @see sim
    */
  def sim(a:Seq[String], b:Seq[String]):Option[Similarity] = {
    if (!contains(a) || !contains(b)) {
      None
    } else {
      def filterFirstSense[A <: Node](set:Set[A], p:Seq[String]):Set[A] = {
        val candidate = set.filter{ (node:Node) =>
          node match {
            case (rn:RealNode) =>
              rn.synset.getWordForms()(0).toLowerCase == p.mkString(" ").toLowerCase
            case _ => true
          }
        }
        if (candidate.size > 0) candidate else set
      }
      Some(sim(filterFirstSense(get(a).toSet, a), filterFirstSense(get(b).toSet, b)))
    }
  }
  
  /** Returns the similarity between two phrases.
    * If a phrase is not in WordNet, its head word is queried.
    * @see sim
    */
  def sim(a:Sentence, b:Sentence, backoff:Boolean=true):Option[Similarity] = {
    if (a.length == 0 || b.length == 0) return None
    if (backoff) {
      // -- Case: Massage Sentences
      for { seqA:Seq[String] <- Util.massage(a, contains(_))
            seqB:Seq[String] <- Util.massage(b, contains(_))
            s:Similarity <- sim(seqA, seqB) } yield s
    } else {
      // -- Case: Exact Match
      sim(a.words, b.words)
    }
  }

  /** @see sim */
  def sim(a:String, b:String):Option[Similarity]
    = sim(a.split("""\s+"""), b.split("""\s+"""))

  /**
   * Maps the ontology, and updates totalCount, etc to be consistent with
   * the mapping. This only applies to real nodes, not the fake roots.
   * The function also has access to the phrase the node is being accessed from.
   * @return A new ontology, with the function applied to each node.
   */
  def mapWithPhrase(fn:(Phrase,RealNode,Option[RealNode])=>RealNode):Ontology = {
    startTrack("Mapping ontology")
    val cache = HashMap[RealNode,RealNode]()
    val (newOntology, newCount, visitedNodes)
      = ontology.keys.foldLeft((Map[Phrase,Set[RealNode]](), 0L, Set[Node]())) {
          case ((ontology:Map[Phrase,Set[RealNode]], count:Long,
                 visited:Set[Node]), phrase:Phrase) =>
        val newRealNodes:Set[RealNode] = this.ontology(phrase).map{
          (n:RealNode) => {
            val out = fn(phrase, n, cache.get(n)); cache(n) = out; out;
          }
        }
        (ontology +( (phrase, newRealNodes) ),
         count + newRealNodes.foldLeft(0L){ case (c:Long, n:RealNode) =>
           c + { if(visited.contains(n)) 0L else n.count }
         },
         visited ++ newRealNodes)
      }
    endTrack("Mapping ontology")
    Ontology(newOntology, root, posRoots)
  }

  /**
   * Maps the ontology, and updates totalCount, etc to be consistent with
   * the mapping. This only applies to real nodes, not the fake roots.
   * @return A new ontology, with the function applied to each node.
   */
  def map(fn:(RealNode,Option[RealNode])=>RealNode):Ontology = {
    mapWithPhrase( (p:Phrase, n:RealNode, cache:Option[RealNode]) => fn(n, cache) )
  }

  /**
   * Performs an action for each node of the ontology. Note that immutable
   * elements cannot be changed.
   */
  def foreach(fn:RealNode=>Any):Unit = map( (n:RealNode, cache:Option[RealNode])
                                               => { fn(n); n } )
  
  /**
   * Performs an action for each node of the ontology, paired with its phrase.
   * Note that immutable elements cannot be changed.
   */
  def foreachWithPhrase(fn:(Phrase,RealNode)=>Any):Unit
    = mapWithPhrase( (p:Phrase, n:RealNode, cache:Option[RealNode])
                        => { fn(p, n); n } )


  //-------------------------
  // Creating Ontology
  //-------------------------

  /**
   * Add a phrase, and its associated synsets to the ontology
   * @return A new ontology, with the phrase added
   */
  private def add(phrase:Phrase, synsets:List[Synset]):Ontology = {
    // -- Add Synsets
    synsets.foldLeft(this){ case (dag:Ontology, synset:Synset) =>
      val node = new RealNode(synset, 0, Set[Node]())
      Ontology(
          if (dag.ontology.contains(phrase))
            dag.ontology + ( (phrase, dag.ontology(phrase) + node) )
          else
            dag.ontology + ( (phrase, Set(node)) ),
          root, posRoots)
    }
  }

  /**
   * Add all synsets for a phrase to the ontology
   * @return A new ontology, with the phrase added
   */
  def ++(phrase:Phrase):Ontology = {
    // -- Get Synsets
    val synsets = SynsetType.ALL_TYPES.foldLeft(List[Synset]()){
        case (synsets:List[Synset], synsetType:SynsetType) =>
      Ontology.wordnet.getSynsets(phrase.mkString("_"),
                                  synsetType).toList ::: synsets
    }
    this.add(phrase, synsets)
  }

  /**
   * Add all synsets for all phrases in a collection to the ontology
   * @return A new ontology, with the phrases added
   */
  def ++(phrases:Iterable[Phrase]):Ontology = {
    startTrack("Populating ontology")
    var count:Int = 0
    val mapped = mapreduceSinglecore( 
      {(phrase:Phrase) => 
        log("loaded [" + count + "]: " + phrase.mkString(" "))
        count += 1
        (phrase, SynsetType.ALL_TYPES.foldLeft(List[Synset]()){
            case (synsets:List[Synset], synsetType:SynsetType) =>
          Ontology.wordnet.getSynsets(phrase.mkString("_"),
                                      synsetType).toList ::: synsets
        })},
      phrases,
      {(dag:Ontology, pair:(Phrase,List[Synset])) => dag.add(pair._1, pair._2)},
      this)
    endTrack("Populating ontology")
    mapped
  }

  /**
   * Declare hypernyms (ontology hypernyms) of a synset.
   * @param fn A mapping from Synset to its hypernyms in the ontology.
   * @return A new ontology, linked with the hypernym relation and rooted
   *         appropriately.
   */
  def declareHypernyms(fn:Synset=>Iterable[Synset]):Ontology = {
    startTrack("Declaring ontology hypernyms")
    // -- Create cache
    startTrack("Creating cache")
    val cache = ontology.values.map{ 
      case (n:Set[RealNode]) => n.map{ n => (n.synset, n) } }.flatten.toMap
    endTrack("Creating cache")
    // -- Create links
    startTrack("Creating links")
    case class MutableNode(node:RealNode,
                           hypernyms:HashSet[MutableNode],
                           hyponyms:HashSet[MutableNode]) {
      override def hashCode:Int = node.hashCode
      override def equals(o:Any):Boolean = o match {
        case (m:MutableNode) => node == m.node
        case _ => false
      }
    }
    var mutableNodes = HashMap[RealNode, MutableNode]()
    cache.foreach{ case (synset:Synset, node:RealNode) =>
      // (apply function)
      val hypernyms = fn(synset)
      // (ensure mapping)
      if (!mutableNodes.contains(node)) {
        mutableNodes(node) = MutableNode(node,
                                             HashSet[MutableNode](), 
                                             HashSet[MutableNode]())
      }
      val mutableChild = mutableNodes(node)
      // (for each hypernym...)
      hypernyms.foreach{ (hypernymSynset:Synset) =>
        if (hypernymSynset == synset) {
          throw fail("Trivial cycle in Ontology")
        } else if (!cache.contains(hypernymSynset)) {
          warn(FORCE, "Could not find hypernym synset: " + hypernymSynset)
        } else {
          // (ensure hypernym)
          val hypernymRealNode = cache(hypernymSynset)
          if (!mutableNodes.contains(hypernymRealNode)) {
            mutableNodes(hypernymRealNode) = MutableNode(hypernymRealNode,
                                                   HashSet[MutableNode](), 
                                                   HashSet[MutableNode]())
          }
          val mutablehypernym = mutableNodes(hypernymRealNode)
          // (update links)
          mutableChild.hypernyms += mutablehypernym
          mutablehypernym.hyponyms += mutableChild
        }
      }
    }
    endTrack("Creating links")
    // -- Find Roots
    forceTrack("Finding roots")
    // (find roots)
    val roots:HashSet[MutableNode] = HashSet[MutableNode]()
    val leaves:HashSet[MutableNode] = HashSet[MutableNode]()
    mutableNodes.values.foreach{ (mutableNode:MutableNode) =>
      if (mutableNode.hypernyms.isEmpty) {
        roots += mutableNode
      }
      if (mutableNode.hyponyms.isEmpty) {
        leaves += mutableNode
      }
    }
    // (print roots)
    if (roots.isEmpty) {
      throw fail("No roots in ontology")
    } else {
      startTrack("Ontology roots")
      roots.foreach{ x =>
        x.node.synset match {
          case (noun:NounSynset) => log(x.node)
          case _ => 
        }
      }
      log("" + roots.size + " total")
      endTrack("Ontology roots")
      log("" + leaves.size + " leaves")
    }
    endTrack("Finding roots")
    // -- Break Cycles
    forceTrack("Breaking cycles")
    var knownBroken = HashSet[Synset]()
    def breakCycle(mutableNode:MutableNode, seen:Set[Synset]):Unit = {
      if (!knownBroken(mutableNode.node.synset)) {
        // (break cycles here)
        mutableNode.hypernyms.retain{ (hypernym:MutableNode) =>
          if (seen(hypernym.node.synset)) {
            hypernym.hyponyms.remove(mutableNode)
            if (hypernym.hyponyms.isEmpty) leaves += hypernym
            warn("broke cycle " + mutableNode.node.synset.getWordForms()(0) +
                 " <- " + hypernym.node.synset.getWordForms()(0))
            false
          } else {
            true
          }
        }
        if (mutableNode.hypernyms.isEmpty) roots += mutableNode
        // (recurse up the tree)
        mutableNode.hypernyms.foreach{ 
          breakCycle(_, seen + mutableNode.node.synset)
        }
        // (cache)
        knownBroken += mutableNode.node.synset
      }
    }
    leaves.clone.foreach{ x => breakCycle(x, Set[Synset]()) }
    endTrack("Breaking cycles")
    // -- Root POS and global root
    forceTrack("Creating global root")
    // (create global roots)
    val posRoots:Map[SynsetType,Node]
      = SynsetType.ALL_TYPES.map{ (typ:SynsetType) =>
        (typ, new AbstractNode(typ.toString, Set[Node](root)))
      }.toMap
    posRoots.foreach{ root.hyponyms += _._2 }
    endTrack("Creating global root")
    // -- Link hypernyms
    forceTrack("Linking hypernyms")
    var linkedWithHypernym = HashMap[Synset, RealNode]()
    def linkHypernym(mutableNode:MutableNode, path:List[Synset]):RealNode = {
      if (path.contains(mutableNode.node.synset)) {
        throw fail("Cycle in graph; term=" + mutableNode.node.synset +
                   " path=" + path)
      }
      // (recursive case)
      if (linkedWithHypernym.contains(mutableNode.node.synset)) {
        // (cached response)
        linkedWithHypernym(mutableNode.node.synset)
      } else {
        val hypernyms:Set[Node] = 
          if (mutableNode.hypernyms.isEmpty) {
            val subroot:Node = posRoots(mutableNode.node.synset.getType)
            Set[Node](subroot)
          } else {
            mutableNode.hypernyms.map{ (p:MutableNode) =>
              linkHypernym(p, mutableNode.node.synset :: path)
            }
          }.toSet
        // (link this)
        val linked = new RealNode(mutableNode.node.synset,
                                  mutableNode.node.count,
                                  hypernyms)
        linkedWithHypernym(mutableNode.node.synset) = linked
        linked
      }
    }
    val linkedLeaves = leaves.map{ x => linkHypernym(x, Nil) }
    endTrack("Linking hypernyms")
    // -- Return
    endTrack("Declaring ontology hypernyms")
    Ontology(
      ontology.map{ case (phrase:Phrase, nodes:Set[RealNode]) =>
        (phrase, nodes.map{ n => linkedWithHypernym(n.synset) })
      },
      root, posRoots.values.toSet
    ).linkHyponymsInPlace
  }

  /**
   * Link the hyponyms of an ontology based on the hypernym hierarchy.
   * @return This same ontology, with hyponyms linked.
   */
  def linkHyponymsInPlace:Ontology = {
    foreach( (n:RealNode) => n.hypernyms.foreach{ _.hyponyms += n } )
    posRoots.foreach( root.hyponyms += _ )
    this
  }

  override def toString:String = {
    ontology.foldLeft(new StringBuilder){
        case (b:StringBuilder, (phrase:Phrase, nodes:Set[RealNode])) =>
      nodes.foldLeft(b){ case (b:StringBuilder, node:RealNode) =>
        b.append(phrase.mkString(" "))
         .append(" [").append(node.count).append("] -> ")
        node.hyponyms.foldLeft(b){ case (b:StringBuilder, child:RealNode) =>
          b.append(child).append(" ")
        }
        b.append("\n")
      }
    }.toString
  }
}

object Ontology {

  //-------------------------
  // Utility Methods
  //-------------------------

  private val wordnet = WordNetDatabase.getFileInstance(); 

  val empty:Ontology
    = Ontology(Map[Phrase, Set[RealNode]](),
               new AbstractNode("ROOT", Set[Node]()), Set[Node]())
  
  /**
   * Load an ontology from a file, re-linking necessary links and checking the
   * ontology's validity
   * @param filename The file to read the model from
   * @return The loaded ontology
   */
  def load(filename:String):Ontology = {
    startTrack("Loading ontology")
    // (read raw ontology)
    val ontology:Ontology = Util.readObject(filename)
    log("read file: " + filename)
    // (relink hyponyms)
    val linkedOntology = ontology.linkHyponymsInPlace
    log("sanity checks passed")
    endTrack("Loading ontology")
    linkedOntology
  }
  
  /**
   *  A node in the ontology. This should never be created directly,
   *  but rather should be instantiated through e.g., RealNode or
   *  AbstractNode.
   */
  @SerialVersionUID(2l)
  class Node(val count:Long, val hypernyms:Set[Node]) extends Serializable {
    /**
     * Direct hyponyms of this node
     */
    val hyponyms:HashSet[Node] = HashSet[Node]()
    
    /**
     * All hyponyms of this node
     */
    def hyponymsRecursive:Set[RealNode] = {
      val frontier = Queue[Node](this)
      var hyponyms = HashSet[RealNode]()
      while (!frontier.isEmpty) {
        val cand = frontier.dequeue
        cand match {
          case (rn:RealNode) =>
            hyponyms = hyponyms += rn
            frontier ++= rn.hyponyms
          case _ => /* do nothing */
        }
      }
      hyponyms.toSet
    }

    lazy val subtreeCount:Long = hyponyms.foldLeft(count){
        case (countSoFar:Long, child:Node) =>
      if (!child.hypernyms.contains(this)) {
        throw new IllegalStateException(
          "" + this + " -> " + child + "; child did not register hypernym")
      }
      countSoFar + child.subtreeCount
    }
    
    override def equals(o:Any):Boolean = throw fail("Abstract node created")
    override def hashCode:Int = throw fail("Abstract node created")
    override def toString:String = throw fail("Abstract node created")
  }
  
  //-------------------------
  // Classes
  //-------------------------
  
  /**
   *  An abstract node, which does not have a corresponding phrase or synset,
   *  but is nonetheless in the tree (e.g., the root node)
   */
  @SerialVersionUID(1l)
  class AbstractNode(val name:String,
                     hypernyms:Set[Node]) extends Node(0L, hypernyms) {
    override def equals(o:Any):Boolean = o match {
      case (n:AbstractNode) => n.name == this.name
      case _ => false
    }

    override def hashCode:Int = name.hashCode

    override def toString:String = name
  }

  /**
   *  An ontology node which corresponds to a WordNet synset and a corresponding
   *  phrase.
   */
  @SerialVersionUID(1l)
  class RealNode(val synset:Synset, count:Long,
                 hypernyms:Set[Node]) extends Node(count, hypernyms) {
    override def equals(o:Any):Boolean = o match {
      case (n:RealNode) => n.synset == this.synset
      case _ => false
    }
    
    def toString(prefer:Set[String]):String = {
      val original:Map[String,String] = prefer.map{ x => (x.toLowerCase, x) }.toMap
      val matches
        = (synset.getWordForms().map( _.toLowerCase ).toSet &
           original.keys.toSet).toList
      if (matches.isEmpty || original(matches(0)) == toString) {
        toString
      } else {
        original(matches(0)) + " [" + toString + "]"
      }
    }

    override def hashCode:Int = synset.hashCode
    override def toString:String = synset.getWordForms()(0)
  }
  
  //-------------------------
  // Similarity
  //-------------------------
    
  /**
   * An encapsulation of similarity functions on an ontology
   */
  @SerialVersionUID(2l)
  case class Similarity(a:Set[Node], b:Set[Node], totalCount:Long) {
    /*
     * Parameters
    */

    /*
     * Cached values for similarity computation
    */
    lazy val (lcs, lcs2leafA, lcs2leafB, a2lcsDist, b2lcsDist, leafA, leafB
        ):(Node,List[Node],List[Node],Int,Int,Node,Node) = {
      // -- Hypernymy Chain
      case class Explorer(path:List[Node]) {
        def head:Node = path.head
        def source:Node = path.last
        def length:Int = path.length - 1
        def ::(n:Node):Explorer = new Explorer(n :: path)
        def toList:List[Node] = path
        override def equals(o:Any):Boolean = o match {
          case (e:Explorer) => e.head == head
          case _ => false
        }
        override def toString:String = "[" + length + "] " + head
        override def hashCode:Int = head.hashCode
      }
      // -- Expand Hypernyms
      def expand(seed:Set[Node]):HashSet[Explorer] = {
        val frontier = Queue[Explorer]()
        val hypernyms = HashSet[Explorer]()
        frontier ++= seed.map{ x => new Explorer(List[Node](x)) }
        hypernyms ++= frontier
        while (!frontier.isEmpty) {
          val frontierNode:Explorer = frontier.dequeue
          frontierNode.head.hypernyms.foreach{ (hypernym:Node) =>
            val newExplorer = hypernym :: frontierNode
            hypernyms += newExplorer
            frontier.enqueue(newExplorer)
          }
        }
        hypernyms
      }
      // -- Find LCS
      // (expand hypernyms)
      val hypernymsA = expand(a)
      val hypernymsB = expand(b)
      // (find intersection trees)
      val intersections = (hypernymsA & hypernymsB)
      val filteredHypernymsA = hypernymsA.filter( intersections(_) )
      val filteredHypernymsB = hypernymsB.filter( intersections(_) )
      filteredHypernymsA.map { (eA:Explorer) =>
        val eB = filteredHypernymsB.filter( _.head == eA.head ).minBy( _.length)
        (eA.head, eA.toList, eB.toList,
         eA.length, eB.length, eA.source, eB.source)
      }.minBy( x => (x._4 + x._5, -x._1.subtreeCount) )
    }

    lazy val root2lcsDist:Double = {
      val frontier = Queue[(Node,Int)]()
      var depth:Int = -1
      frontier.enqueue((lcs, 0))
      while (!frontier.isEmpty) {
        val (hyponym, dist) = frontier.dequeue
        hyponym.hypernyms.foreach{ (n:Node) =>
          if (n.hypernyms.isEmpty) depth = dist
          frontier.enqueue( (n, dist + 1) )
        }
      }
      if (depth >= 0) depth.toDouble else 1.0 // TODO(gabor) this shouldn't happen
    }

    lazy val logProbLCS
      = scala.math.log( lcs.subtreeCount.toDouble / totalCount.toDouble )
    
    lazy val logProbA
      = scala.math.log( leafA.subtreeCount.toDouble / totalCount.toDouble )
    
    lazy val logProbB
      = scala.math.log( leafB.subtreeCount.toDouble / totalCount.toDouble )

    /**
     * Compute the Lesk overlap between two nodes, centered only on this node.
     * For example, the value given the sentences [a, b, c, d] and [a, c, d, e]
     * would be 1^2 2^2 = 5 (for 'a' and 'c d').
     * This value is normalized to be between 0 and 1 by dividing by the
     * square of the length of the shorter definition (the maximum possible
     * value of the metric).
     * TODO(gabor) I'm playing with fire a bit here in terms of implementation
     *             complexity.
     */
    def lesk(a:RealNode, b:RealNode, approx:Boolean=false):Double = {
      import Search._
      def allEqual(a:Array[Symbol], startA:Int,
                   b:Array[Symbol], startB:Int, length:Int):Boolean = {
        (0 until length).forall{ (i:Int) => a(startA + i) == b(startB + i) }
      }
      def allFalse(mask:Array[Boolean], start:Int, untilVal:Int):Boolean = {
        (start until untilVal).forall( mask(_) == false )
      }
      // (variables)
      val tokensA:Array[Symbol]
        = a.synset.getDefinition.toLowerCase.split("""\s+""").map( Symbol(_) )
      val tokensB:Array[Symbol]
        = b.synset.getDefinition.toLowerCase.split("""\s+""").map( Symbol(_) )
      val tokensShort = if (tokensA.length < tokensB.length) tokensA else tokensB
      val tokensLong = if (tokensA.length < tokensB.length) tokensB else tokensA
      // (possible alignments)
      var candidates = List[((Array[Boolean],Array[Boolean])=>Boolean,
                     AlignState=>AlignState)]()
      for( length <- 1 to tokensB.length;
           shortStart <- 0 to tokensShort.length - length;
           longStart <- 0 to tokensLong.length - length ) {
        if (allEqual(tokensShort, shortStart,
                     tokensLong, longStart, length) ) {
          val candidate = (
            (shortMask:Array[Boolean], longMask:Array[Boolean]) => {
              allFalse(shortMask, shortStart, shortStart + length) &&
              allFalse(longMask, longStart, longStart + length)
            },
            (old:AlignState) => {
              val newShortMask = old.shortMask.map( x => x )
              val newLongMask = old.longMask.map( x => x )
              for( i <- shortStart until shortStart + length ) newShortMask(i) = true
              for( i <- longStart until longStart + length ) newLongMask(i) = true
              new AlignState(newShortMask, newLongMask, old.cost - length * length)
            }
          )
          candidates = candidate :: candidates
        }
      }
      // (search)
      case class AlignState(shortMask:Array[Boolean],
                            longMask:Array[Boolean],
                            override val cost:Double) extends SearchState {
        override def children:List[AlignState] = {
          candidates.filter( _._1(shortMask, longMask) )
                    .map( _._2(this) )
        }
      }
      val maxCost:Double = tokensShort.length.toDouble * tokensShort.length.toDouble
      val cost = (new Search[AlignState](if (approx) GREEDY else cache(UNIFORM_COST)))
          .best(AlignState(tokensShort.map( x => false ),
                           tokensLong.map( x => false ),
                           maxCost)).cost
      val overlap:Double = maxCost - cost
      overlap / maxCost
    }

    /*
     * Similarity measure implementations
     */

    /**
     * -log(pathlen(A \cap B))
     */
    lazy val path:Double = {
      if (a2lcsDist + b2lcsDist == 0) Double.PositiveInfinity
      else -scala.math.log((a2lcsDist + b2lcsDist).asInstanceOf[Double])
    }

    /**
     * -log( P(A \cap B) )
     */
    lazy val resnik:Double = -logProbLCS

    /**
     * log( P(A \cap B) ** 2) / log( P(A) * P(B) )
     */
    lazy val lin:Double = 2.0 * logProbLCS / (logProbA + logProbB)

    /**
     *    1 / log( P(A \cap B) ** 2 / ( P(A) * P(B) ) )
     *//*          ^  1 if same; more otherwise  ^
     *        ^    0 if same; more otherwise       ^
     *    ^       inf if same; 0 if different        ^
     */
    lazy val jc:Double = {
      val result = 1.0 / (2.0 * logProbLCS - (logProbA + logProbB))
      if (result.isNaN) 0.0 else result
    }

    /**
     * 2 * depth(lcs) / ( pathlen(a, lcs) + pathlen(b, lcs) + 2 * depth(lcs) )
     */
    lazy val wuPalmer:Double
      = 2.0 * root2lcsDist / (a2lcsDist + b2lcsDist + 2.0 * root2lcsDist)

    /**
     * max_{sa \in synsets(a), sb \in synsets(b)} overlap(gloss(sa), gloss(b))
     * This value is normalized to be between 0 and 1.
     */
    lazy val lesk:Double = {
      a.map{ case (nodeA:RealNode) =>
        b.map{ case (nodeB:RealNode) =>
          lesk(nodeA, nodeB)
        }
      }.flatten.max
    }
    
    /**
     * An approximation to the (otherwise slower) lesk algorithm.
     * @see lesk
     */
    lazy val approximateLesk:Double = {
      a.map{ case (nodeA:RealNode) =>
        b.map{ case (nodeB:RealNode) =>
          lesk(nodeA, nodeB, true)
        }
      }.flatten.max
    }

    /**
     * Same as lesk, but measures overlap for all hyponyms as well.
     * This value is normalized to be between 0 and 1.
     */
    lazy val eLesk:Double = {
      // (construct hypornyms)
      val hyponymsA = a.foldLeft(HashSet[RealNode]()){
          case (set:HashSet[RealNode], node:RealNode) => set | node.hyponymsRecursive } // union
      val hyponymsB = b.foldLeft(HashSet[RealNode]()){
          case (set:HashSet[RealNode], node:RealNode) => set | node.hyponymsRecursive } // union
      // (aggregate)
      a.map{ (nodeA:Node) =>
        b.map{ (nodeB:Node) =>
          var overlap:Double = 0.0
          var total:Int = 0
          for( hypA:RealNode <- nodeA.hyponymsRecursive; 
               hypB:RealNode <- nodeB.hyponymsRecursive ) {
            overlap += lesk(hypA, hypB)
            total += 1
          }
          if (total > 0) overlap / total.toDouble else 0.0
        }
      }.flatten.max
    }

    override def toString:String = {
      "" + a.maxBy( _.subtreeCount ) + " vs. " + b.maxBy( _.subtreeCount )
    }
  }
}
