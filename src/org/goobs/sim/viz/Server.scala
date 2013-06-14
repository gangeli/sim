package org.goobs.sim.viz

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

import java.util.{HashMap => JMap}

import org.goobs.net._
import org.goobs.net.JsonHandler.JSONBuilder
import org.goobs.net.WebServer.HttpInfo

import edu.stanford.nlp.util.logging.StanfordRedwoodConfiguration
import edu.stanford.nlp.util.logging.Redwood
import edu.stanford.nlp.util.logging.Redwood.Util.{ITALIC, forceTrack, GREEN, YELLOW}
import edu.stanford.nlp.util.logging.Redwood.{startTrack, endTrack}

import org.json._

import org.goobs.sim._
import org.goobs.sim.Util.Phrase
import org.goobs.sim.Ontology.Node
import org.goobs.sim.Ontology.RealNode

import edu.smu.tspell.wordnet.SynsetType

import breeze.linalg._

//------------------------------------------------------------------------------
// Backend
//------------------------------------------------------------------------------
object Backend {

  //--------------------
  // Query Dictionary
  //--------------------

  case class TrieResult(wordMatch:Boolean, prefixMatch:Boolean) {
    Redwood.log(ITALIC, "TrieResult: word_in_dictionary=" + wordMatch +
        "; prefix_in_dictionary=" + prefixMatch)
    def json:JSONObject = {
      val obj = new JSONObject()
      obj.put("word_in_dictionary", wordMatch)
      obj.put("prefix_in_dictionary", prefixMatch)
      obj
    }
  }

  def trieLookup(partialWord:String)
                (implicit wordnetTrie:Trie, distsim:DistSim):TrieResult
    = TrieResult(wordnetTrie(partialWord) && distsim.contains(partialWord),
                 wordnetTrie.prefix(partialWord))
  
  //--------------------
  // Query Similarity
  //--------------------
  case class DistanceInfo(nodes:Array[ProjectedWord], tree:Tree) {
    Redwood.log(ITALIC, "DistanceInfo: " + nodes.length + " nodes")
    def json:JSONObject = {
      val obj = new JSONObject()
      obj.put("nodes", new JSONArray(nodes.map( _.json).filter( _ != null )))
      obj.put("tree", tree.json)
      obj
    }
  }
  
  type WordVector = DenseVector[Double]

  case class ProjectedWord(word:String, x:Double, y:Double, dist:Double,
                           similarityToBasis:Array[Double],
                           lambda:Array[Double]) {
    assert(similarityToBasis.forall( !_.isNaN ))
    assert(lambda.forall( !_.isNaN ))

    def json:JSONObject = {
      val obj = new JSONObject()
      obj.put("word", word)
      obj.put("x", x)
      obj.put("y", y)
      obj.put("dist", dist)
      obj.put("sim_to_basis", new JSONArray(similarityToBasis.map{ (sim:Double) =>
        if (sim.isPosInfinity) 999.0 else if (sim.isNegInfinity) -999.0 else sim
      }))
      obj.put("lambda", new JSONArray(lambda))
      obj
    }
  }
  
  case class Tree(value:String, synsetHash:Int,
                  index:Option[Int], children:Array[Tree]) {
    def cleanValueString:String = {
      value match {
        case "1" => "Noun"
        case "2" => "Verb"
        case "3" => "Adjective"
        case "ROOT" => "Concept"
        case _   => value
      }
    }

    def json:JSONObject = {
      val root = new JSONObject()
      root.put("value", cleanValueString)
      root.put("synset", synsetHash)
      index match {
        case Some(i) => root.put("index", i)
        case None    =>
      }
      root.put("children", new JSONArray( children.map{ _.json } ))
      root
    }
  }

  def makeTree(words:Array[String], indices:Map[String,Int], algorithm:String)
              (implicit wordnet:Ontology, distsim:DistSim):Tree = {
    if (words.length != 3) {
      throw new IllegalArgumentException("Cannot build spanning tree with basis != 3")
    }
    // -- Get Similarities
    // (similarities)
    val sims = List(
             (Similarity(words(1), words(0)), Similarity(words(0), words(2))),
             (Similarity(words(0), words(1)), Similarity(words(1), words(2))),
             (Similarity(words(1), words(2)), Similarity(words(2), words(0))))
               .sortBy{ case (a, b) => math.max(a(algorithm), b(algorithm)) }
               .map{ case (a, b) => (a.wordnet, b.wordnet) }
    val (simABOption, simBCOption) = sims.filter{ case (a, b) => a.isDefined && b.isDefined }.head
    for ( simAB <- simABOption;
          simBC <- simBCOption ) {
      val simABC = wordnet.sim(simAB.lcs, simBC.lcs)
      // (paths)
      val pathToA = simABC.lcs2leafA ::: simAB.lcs2leafA.tail
      val pathToC = simABC.lcs2leafB ::: simBC.lcs2leafB.tail
      val pathToB = {
        val left  = simABC.lcs2leafA ::: simAB.lcs2leafB.tail
        val right = simABC.lcs2leafB ::: simBC.lcs2leafA.tail
        val overlapLeft  = left.zip(pathToA).takeWhile( x => x._1 == x._2).length
        val overlapRight = right.zip(pathToC).takeWhile( x => x._1 == x._2).length
        if (overlapLeft >= overlapRight) left else right
      }
      // -- Build Tree
      // (helper functions)
      def index(string:String):Option[Int] = {
        val AugmentedWord = """^([^\[]+)\s+\[.*\]$""".r
        string match {
          case AugmentedWord(word) => indices.get(word)
          case _ => indices.get(string)
        }
      }
      def buildTree(value:Node, paths:List[List[Node]]):Tree = {
        val (valueString, hashCode) = value match {
          case (r:RealNode) => (r.toString(indices.keys.toSet),
                                System.identityHashCode(r))
          case _ => (value.toString, 0)
        }
        if (paths.forall(_.isEmpty)) {
          Tree(valueString, hashCode, index(valueString), Array[Tree]())
        } else {
          val branches:Set[Node] = paths.map( _.head ).toSet
          val children:Array[Tree] = branches.map{ (childsHead:Node) =>
            buildTree(childsHead,
                      paths.filter( _.head == childsHead)
                           .map ( _.tail )
                           .filter( !_.isEmpty ))
          }.toArray
          Tree(valueString, hashCode, index(valueString), children)
        }
      }
      // (start it off)
      val paths = List(pathToA, pathToB, pathToC)
      if (paths.map( _.head ).toSet.size != 1) {
        throw new IllegalStateException("algorithm error (bad root)")
      }
      return buildTree(paths(0).head, paths.map( _.tail ).filter( !_.isEmpty ))
    }
    return Tree("<no match>", "<no match>".hashCode, None, Array[Tree]())
  }

  def mkPlane(vectors:Array[WordVector], basis:Array[Int]
      ):(DenseVector[Double], DenseVector[Double], DenseVector[Double]) = {
    if (basis.length != 3) {
      throw new IllegalArgumentException("Basis must be 3 elements")
    }
    val basisVectors = basis.map( vectors(_) )
    val planeVector1 = basisVectors(1) - basisVectors(0)
    val planeBasis1  = planeVector1 / norm(planeVector1, 2)
    val planeVector2 = basisVectors(2) - basisVectors(0)
    val planeBasis2Unnormalized = planeVector2 -
                         (planeBasis1 :* (planeVector2.t * planeBasis1))
    val planeBasis2  = planeBasis2Unnormalized / norm(planeBasis2Unnormalized,2)
    (basisVectors(0), planeBasis1, planeBasis2)
  }

  def barycentricProjection(words:Array[String],
                            vectors:Array[WordVector],
                            basis:Array[Int],
                            algorithm:String)
                            (implicit wordnet:Ontology, distsim:DistSim):Array[ProjectedWord] = {
    val coords:Array[Option[(Double,Double)]] = words.map{ x => None }
    // -- Create Triangle
    // (similarities)
    val vertices = basis.map( words(_) )
    val s12:Double = Similarity(vertices(0), vertices(1)).normalizedSimilarity(algorithm)
    val s13:Double = Similarity(vertices(0), vertices(2)).normalizedSimilarity(algorithm)
    val s23:Double = Similarity(vertices(1), vertices(2)).normalizedSimilarity(algorithm)
    // (vertices)
    val edges = List((s12, 0, 1), (s13, 0, 2), (s23, 1, 2)).sortBy( - _._1 )
    val (d12, originI, xI) = edges(0)
    coords(basis(originI)) = Some((0,0))
    coords(basis(xI)) = Some((0,d12))
    val (d13, d23, yI) =
      if (originI == 0 && xI == 1)      (s13, s23, 2)
      else if (originI == 0 && xI == 2) (s12, s23, 1)
      else if (originI == 1 && xI == 0) (s23, s13, 2)
      else if (originI == 1 && xI == 2) (s12, s23, 0)
      else if (originI == 2 && xI == 0) (s23, s12, 0)
      else if (originI == 2 && xI == 1) (s13, s12, 0)
      else throw new IllegalArgumentException("Invalid state")
    // (third coordinate)
    val x = (d13 + d12*d12 - d23) / (2.0 * d12)
    val y = math.sqrt( math.max(0, d13 - (d13 + d12 - d23)*(d13 + d12 - d23) / (4.0 * d12 * d12)))
    coords(basis(yI)) = Some((x,y))
    // -- Calculate similarities
    val lambdas:Array[Array[Double]] = words.map{ (word:String) =>
      val similarity = basis.map{ (basisIndex:Int) =>
        Similarity(word, words(basisIndex)).normalizedSimilarity(algorithm)
      }.map( x => if (x.isNaN) 0.0 else x )
      similarity.map{ x => if(similarity.sum == 0.0) 1.0 / basis.length.toDouble
                           else if (x == Double.PositiveInfinity) 1.0
                           else x / similarity.sum }
    }
    val similarities:Array[Array[Double]] = words.map{ (word:String) =>
      basis.map{ (basisIndex:Int) =>
        Similarity(word, words(basisIndex)).apply(algorithm)
      }
    }
    // -- Project projections
    words.zip(similarities).zip(lambdas).zipWithIndex.map{
        case (((w:String, sim:Array[Double]), lambda:Array[Double]), i:Int) =>
      var x:Double = 0
      var y:Double = 0
      coords(i) match {
        case Some((realX, realY)) => x = realX; y = realY
        case None =>
      }
      val dist:Double = 0
      ProjectedWord(words(i), x, y, dist, sim, lambda)
    }
  }
  
  def wordDistance(algorithm:String, words:Array[String],
                   basis:Array[Int])
                   (implicit wordnet:Ontology, distsim:DistSim):DistanceInfo = {
    // -- Calculate Variables
    val indices:Map[String,Int] = words.map( _.toLowerCase ).zipWithIndex.toMap
    // -- Run Functions
    // (projection)
    val nodes = barycentricProjection(words, words.map( distsim(_) ), basis,
                                 algorithm)
    // (tree)
    val tree:Tree = makeTree(basis.map( words(_) ), indices, algorithm)
    // -- Return
    DistanceInfo(nodes, tree)
  }
  
  //--------------------
  // Define Word
  //--------------------
  case class WordnetDefinition(definition:String) {
    def json:JSONObject = {
      val root = new JSONObject()
      root.put("gloss", definition)
      root
    }
  }

  def wordnetDefinition(word:String, synsetAddress:Int,
                        algorithm:String,basis:Array[String])
                        (implicit wordnet:Ontology, distsim:DistSim):WordnetDefinition = {
    try {
      var synsets = wordnet.get(word).filter( System.identityHashCode(_) == synsetAddress).toList
      if (synsets.size == 0) {
        synsets = wordnet.get(word).toList.sortBy{ (candidate:Node) =>
          -basis.foldLeft(0.0){ case (sim:Double, basisWord:String) =>
            sim + wordnet.sim(Set[Node](candidate), wordnet.get(basisWord)).jc
          }
        }
      }
      synsets(0) match {
        case (r:RealNode) => return WordnetDefinition(r.synset.getDefinition)
      }
    } catch {
      case (e:Exception) => WordnetDefinition("Exception: " + e)
    }
    WordnetDefinition("Could not find '" + word + "' in dictionary")
  }

  //--------------------
  // Query Subtree Words
  //--------------------

  case class SubtreeInfo(children:Set[String],
                         depthFromRoot:Int,
                         depthOfSubtree:Int) {
    Redwood.log(ITALIC, "SubtreeInfo: " + children.size + " children")
    def json:JSONObject = {
      val obj = new JSONObject()
      obj.put("depth_from_root", depthFromRoot)
      obj.put("subtree_depth", depthOfSubtree)
      obj.put("subtree_elements", children)
      obj
    }
  }

  def subtreeSet(word:String)
                (implicit wordnet:Ontology):SubtreeInfo = {
    import scala.collection.mutable.HashSet
    import scala.collection.mutable.Queue
    // -- Variables
    val frontier = Queue[(Node,Int)]()
    frontier ++= wordnet.get(word).map{ (n:Node) => (n, 0) }
    val children = HashSet[String]()
    var maxDepth = 0
    // -- Collect subtree
    while (!frontier.isEmpty()) {
      val (onPrix, depth) = frontier.dequeue
      onPrix.hyponyms.foreach{ case (n:RealNode) =>
        children ++= n.synset.getWordForms
        maxDepth = math.max(maxDepth, depth)
        frontier.enqueue( (n, depth + 1) )
      }
    }
    // -- Find path to root
    def searchUpwards(node:Node):Int = {
      if (node.hypernyms.isEmpty) {
        0
      } else if (node.isInstanceOf[RealNode] &&
                 node.asInstanceOf[RealNode].synset.getType != SynsetType.NOUN) {
        Int.MaxValue  // only search up the noun hierarchy
      } else {
        1 + node.hypernyms.foldLeft(Int.MaxValue){
            case (minDist:Int, parent:Node) =>
          math.min(minDist, searchUpwards(parent))
        }
      }
    }
    val minDistToRoot = wordnet.get(word).foldLeft(Int.MaxValue) {
        case (minDist:Int, node:Node) =>
      math.min(minDist, searchUpwards(node))
    } - 1 // don't include Noun subroot
    // -- Return
    SubtreeInfo(children.toSet, minDistToRoot ,maxDepth)
  }
}

//------------------------------------------------------------------------------
// Server
//------------------------------------------------------------------------------
object Server {
  import Backend._

  implicit def fn2handler(fn:(Map[String,String],HttpInfo)=>String
                         ):WebServerHandler = {
    new JsonHandler(){
      override def handleJSON(values:java.util.HashMap[String,String],
                              info:HttpInfo):String = {
        //(convert hashmap)
        val scalaMap:HashMap[String,String] = new HashMap[String,String]
        val iter = values.keySet.iterator
        while(iter.hasNext) {
          val key = iter.next
          scalaMap(key) = values.get(key)
        }
        //(handle)
        val reply = try{
          var str:String = fn(scalaMap.toMap, info)
          if(!str.contains("{")){ str = "\""+str+"\"" }
          str
        } catch {
          case (r:RuntimeException) => 
            r.printStackTrace
            "ERROR: " + r.getMessage
          case (e:JSONException) =>
            e.printStackTrace
            "ERROR: " + e.getMessage
        }
        reply
      }

      override def handleNonJSON(values:java.util.HashMap[String,String],
                                 info:HttpInfo
                                 ):String = handleJSON(values, info)
    }
  }

  def dict_lookup(args:Map[String,String], info:HttpInfo)
                 (implicit wordnetTrie:Trie, wordnet:Ontology, distsim:DistSim):String = {
    args.get("data") match {
      case Some(jsonString) =>
        val json = new JSONObject(jsonString)
        trieLookup(json.getString("partial_word").trim()).json.toString
      case None => "Expecting GET with 'data=<json_string>'"
    }
  }
  
  def define(args:Map[String,String], info:HttpInfo)
            (implicit wordnet:Ontology, distsim:DistSim):String = {
    args.get("data") match {
      case Some(jsonString) =>
        val json = new JSONObject(jsonString)
        val basis:Array[String] = {
            val jsonArray = json.getJSONArray("basis")
            (0 until jsonArray.length).map{ (i:Int) =>
              jsonArray.getString(i)
            }.toArray
          }
        wordnetDefinition(json.getString("word").trim(),
                          json.getInt("synset"),
                          json.getString("algorithm"),
                          basis
                         ).json.toString
      case None => "Expecting GET with 'data=<json_string>'"
    }
  }

  def word_distance(args:Map[String,String], info:HttpInfo)
                   (implicit wordnet:Ontology, distsim:DistSim):String = {
    args.get("data") match {
      case Some(jsonString) =>
        // -- Parse JSON
        val json = new JSONObject(jsonString)
        val algorithm:String = json.getString("algorithm").trim()
        val words:Array[String] = {
            val jsonArray = json.getJSONArray("words")
            (0 until jsonArray.length).map{ (i:Int) =>
              jsonArray.getString(i)
            }.toArray
          }.map( _.trim() )
        val basis:Array[Int] = {
            val jsonArray = json.getJSONArray("basis")
            (0 until jsonArray.length).map{ (i:Int) =>
              jsonArray.getInt(i)
            }.toArray
          }
        // -- Process Query
        val rtn:String = wordDistance(algorithm, words, basis).json.toString
        if (rtn != null) rtn else "Unknown word distance for " + algorithm + " on " + words + " with basis " + basis
      case None => "Expecting GET with 'data=<json_string>'"
    }
  }

  def subtree(args:Map[String,String], info:HttpInfo)
             (implicit wordnet:Ontology):String = {
    args.get("data") match {
      case Some(jsonString) =>
        val json = new JSONObject(jsonString)
        subtreeSet(json.getString("root")).json.toString
      case None => "Expecting GET with 'data=<json_string>'"
    }
  }
  
  def main(args:Array[String]) = {
    // -- Argument Check
    if (args.length < 2) {
      System.err.println("Expected two arguments: {wordnet_model_path} {distsim_model_path} [port?]")
      System.exit(1)
    }
    // -- Redwood
    val props = new java.util.Properties();
    props.setProperty("log.neatExit", "true");
    props.setProperty("log.captureStderr", "false");
    StanfordRedwoodConfiguration.apply(props);
    startTrack("Setup Server")
    // -- Initialize WordNet
    forceTrack("Reading Wordnet")
    implicit val wordnet:Ontology = Ontology.load(args(0))
    Redwood.log(GREEN, "loaded wordnet")
    implicit val trie = new Trie
    wordnet.foreachWithPhrase{ (phrase:Phrase, node:RealNode) =>
      trie += phrase.mkString(" ")
    }
    wordnet.foreachWithPhrase{ (phrase:Phrase, node:RealNode) =>
      assert(trie(phrase.mkString(" ")),
             "Phrase not in trie: " + phrase.mkString(" "))
    }
    Redwood.log(GREEN, "created trie")
    endTrack("Reading Wordnet")
    // -- Initialize DistSim
    implicit val distsim:DistSim = DistSim.load(args(1))
    Redwood.log(GREEN, "read word vectors")
    // -- Start Server
    startTrack("Starting Server")
    val port:Int = if (args.length < 3) 4280 else args(2).toInt
    new WebServer(port).start
      .register("/dict_lookup",   dict_lookup(_:Map[String,String], _:HttpInfo))
      .register("/word_distance", word_distance(_:Map[String,String], _:HttpInfo))
      .register("/define",        define(_:Map[String,String], _:HttpInfo))
      .register("/subtree",       subtree(_:Map[String,String], _:HttpInfo))
    Redwood.log(YELLOW, "Listening on port " + port)
    Redwood.log(GREEN, "bound uris")
    endTrack("Starting Server")
    endTrack("Setup Server")
    Redwood.log("waiting for connections...")
  }
}
