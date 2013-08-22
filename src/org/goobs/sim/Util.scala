package org.goobs.sim

import scala.collection.JavaConversions._
import scala.collection.mutable.SynchronizedQueue
import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe._
import scala.concurrent._

import java.io.InputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.util.zip.GZIPInputStream

import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp._

//------------------------------------------------------------------------------
// Util Objects
//------------------------------------------------------------------------------
object Util {
  val mainThreadId = Thread.currentThread().getId()

  type Phrase = Seq[String]


  def map[A : Manifest, B : Manifest, This <: TraversableOnce[A], That <: TraversableOnce[B]]
      (f:A=>B, collection:This, chunk:Int = 1)
      (implicit bf: CanBuildFrom[List[B], B, That]): That = {
	  val outputs:Array[B] = 
      if (Thread.currentThread().getId() != mainThreadId){
        collection.toArray.map(f(_))
      } else {
        val outputs:Array[B] = new Array(collection.size)
        // (create runnables for tasks)
        val tasks = collection
                      .toList.zipWithIndex.map{ (pair:(A,Int)) =>
            val (elem, index) = pair
    	      new Runnable() {
    	        override def run:Unit = {
    	          val output = f(elem)
    	          outputs(index) = output
    	        }
    	      }
    	    }
        // (create chunked tasks)
        val chunks = tasks.foldLeft(List[List[Runnable]](List[Runnable]())){
            case (soFar:List[List[Runnable]], term:Runnable) =>
          val head = soFar.head
          val tail = soFar.tail
          if (head.size >= chunk) {
            List[Runnable](term) :: head.reverse :: tail
          } else {
            (term :: head) :: tail
          }
        }
        // (create chunked runnables)
        val chunkedTasks:List[Runnable]
          = (chunks.head.reverse :: chunks.tail).reverse
              .map{ (tasks:List[Runnable]) =>
            new Runnable() {
              override def run:Unit = {
                tasks.foreach( _.run )
              }
            }
          }
        // (map)
    	  threadAndRun(asJavaIterable(chunkedTasks), Runtime.getRuntime.availableProcessors())
        outputs
      }
    // (output result)
    val builder = bf()
    outputs.toList.foreach( builder += _ )
    builder.result
  }

	def mapreduce[A : Manifest, B : Manifest,C](f:A=>B, collection:TraversableOnce[A],
	                                reduce:(C,B)=>C, begin:C,
                                  chunk:Int = 1):C = {
	  map(f, collection, chunk).foldLeft(begin){ (soFar:C, elem:B) =>
      reduce(soFar, elem)
    }
  }

	def mapreduceSinglecore[A,B : Manifest,C](map:A=>B, collection:TraversableOnce[A],
	                                          reduce:(C,B)=>C, begin:C,
                                            chunk:Int = 1):C = {
    collection.map(map).foldLeft(begin){ (c:C, b:B) => reduce(c,b) }
  }

  def readObject[A : Manifest](path:String, track:Option[String] = None):A = {
    track.foreach( forceTrack(_) )
    var inputStream:InputStream = null
    try {
      try {
      inputStream = new GZIPInputStream(
          this.getClass.getClassLoader.getResourceAsStream(path))
      } catch { case (e:Exception) =>
        inputStream = 
          if (path.endsWith(".gz")) {
            new GZIPInputStream(new FileInputStream(path))
          } else {
            new FileInputStream(path)
          }
      }
    } catch {
      case (e:Exception) =>
        err("Could not find "+path+" in either the classpath or filesystem.")
        System.exit(1)
    }
    val obj:A = (new ObjectInputStream(inputStream)).readObject.asInstanceOf[A]
    inputStream.close
    track.foreach( endTrack(_) )
    return obj
  }
  
  def min(nums: Int*): Int = nums.min

  def editDistance[A](a:Seq[A], b:Seq[A]):Int = {
    val lenA = a.length
    val lenB = b.length
    val d: Array[Array[Int]] = Array.ofDim(lenA + 1, lenB + 1)
    for (i <- 0 to lenA) d(i)(0) = i
    for (j <- 0 to lenB) d(0)(j) = j
    for (i <- 1 to lenA; j <- 1 to lenB) {
      val cost = if (a(i - 1) == b(j-1)) 0 else 1
      d(i)(j) = min(
        d(i-1)(j  ) + 1,     // deletion
        d(i  )(j-1) + 1,     // insertion
        d(i-1)(j-1) + cost   // substitution
      )
    }
    d(lenA)(lenB)
  }

  /** Get CPU time in nanoseconds */
  def cpuTime:Long = {
    import java.lang.management._;
    val bean = ManagementFactory.getThreadMXBean
    assert( bean.isThreadCpuTimeSupported )
    bean.getThreadCpuTime( java.lang.Thread.currentThread.getId )
  }

  def toString(i:Int, n:Int):String = {
    val str = i.toString
    val b = new StringBuilder
    while (b.length < (n - str.length) / 2) b.append(" ")
    b.append(str)
    while(b.length < n) b.append(" ")
    b.toString
  }
  
  def replaceNamedEntities(raw:Sentence):Sentence = {
    import edu.stanford.nlp.Magic._
    var filtered = List[String]()
    var lastTag:String = ""
    val input:Sentence = new Sentence(
      filterNonsense(raw).word.map( x => if (x == "$") "dollars" else x))
    if (input.length == 0){
      new Sentence(Array("number"))
    } else {
      for (i <- 0 until input.length) {
        val tag = input.ner(i)
        if (tag != "O") {
          if (tag != lastTag) {
            filtered = tag.toLowerCase :: filtered
          } else { /* duplicate tag */ }
        } else {
          filtered = input.words(i) :: filtered
        }
        lastTag = tag
      }
      new Sentence(filtered.reverse.toArray)
    }
  }

  def massage(s:Sentence, contains:Seq[String]=>Boolean, replaced:Boolean=false,
              acronym:Boolean=false):Option[Seq[String]] = {
    lazy val depluralized:Seq[String]
      = s.word.zip(s.pos).map{ case (w:String, tag:String) =>
        if (w.length > 0 && tag.toLowerCase.startsWith("n") && w.last == 's') {
          w.take(w.length - 1)
        } else{
          w
        }
      }
    lazy val depluralizedHead = {
      val w = s.headWord
      if (w.length > 0 && s.headPOS.toLowerCase.startsWith("n") && w.last == 's') {
        w.take(w.length - 1)
      } else{
        w
      }
    }
    if (contains(s.word)) Some(s.word)
    else if (contains(s.lemma)) Some(s.lemma)
    else if (contains(depluralized)) Some(depluralized)
    else if (contains(List(s.headWord))) Some(List(s.headWord))
    else if (contains(List(s.headLemma))) Some(List(s.headLemma))
    else if (contains(List(depluralizedHead))) Some(List(depluralizedHead))
    else if (!replaced) massage(Util.replaceNamedEntities(s), contains, true)
    else {
//      println("~~~" + s)
      None
    }
  }
  
  def filterNonsense(s:Sentence):Sentence = {
    import ClosedClassWords._
    import edu.stanford.nlp.Magic._
    if (s.words.length == 1) {
      if (s.words(0) == "#") new Sentence(Array("number")) // case: number
      else if (determinerAnimacy.contains(s.words(0)))
        new Sentence(Array(determinerAnimacy(s.words(0)))) // case: nonsense
      else s                                           // case: normal word
    } else {
      if (s.words.contains("#") || s.words.contains("")) {
        new Sentence(s.words.map( x =>
          if      (x == "#") "1" 
          else x 
        ).filter( _ != "" ))
      } else s
    }
  }

}

//------------------------------------------------------------------------------
// Similarity
//------------------------------------------------------------------------------
class Similarity(sentA:Sentence, sentB:Sentence, backoff:Boolean, wordnetInstance:Ontology, distsimInstance:DistSim) {
  import FancyMath.bilinearBinaryTransform

  val wordnet = wordnetInstance.sim(sentA, sentB)
  val distsim = distsimInstance.sim(sentA, sentB)

  // Wordnet similarities
  def path:Double      = wordnet.map( x => x.path     ).getOrElse(Double.NaN)
  def resnik:Double    = wordnet.map( x => x.resnik   ).getOrElse(Double.NaN)
  def lin:Double       = wordnet.map( x => x.lin      ).getOrElse(Double.NaN)
  def jc:Double        = wordnet.map( x => x.jc       ).getOrElse(Double.NaN)
  def wuPalmer:Double  = wordnet.map( x => x.wuPalmer ).getOrElse(Double.NaN)
  def lesk:Double      = wordnet.map( x => x.approximateLesk ).getOrElse(Double.NaN)
  def eLesk:Double     = wordnet.map( x => x.eLesk    ).getOrElse(Double.NaN)

  def cos:Double       = distsim.map( x => x.cos      ).getOrElse(Double.NaN)
  def angle:Double     = distsim.map( x => x.angle    ).getOrElse(Double.NaN)
  def jensenShannon:Double
                       = distsim.map( x => x.jensenShannon ).getOrElse(Double.NaN)
  def hellinger:Double = distsim.map( x => x.hellinger).getOrElse(Double.NaN)
  def jaccard:Double   = distsim.map( x => x.jaccard  ).getOrElse(Double.NaN)
  def dice:Double      = distsim.map( x => x.dice     ).getOrElse(Double.NaN)

  def average:Double = List( path, resnik, lin, jc, wuPalmer, cos, angle,
                             jensenShannon, hellinger, jaccard, dice ).sum / 12.0
  
  /**
   * A normalized similarity between the two concepts.
   * @param algorithm The algorithm to use
   * @return A distance, between 0 and 1.
   *         1 means identical; 0 means polar opposite
   */
  def normalizedSimilarity(algorithm:String):Double = {
    val result = algorithm.toLowerCase match {
      case "path" => 1.0 - scala.math.exp(-path) / (2.0 * wordnetInstance.depth)
      case "resnik" => 1.0 - scala.math.exp(-resnik)
      case "lin" => scala.math.exp( lin - 1.0 )
      case "jc" => if (jc > 1.0) 1.0 else jc
      case "wupalmer" => wuPalmer
      // bilinear (linear fractional; mobius) transform
      // 0 -> 0; 1 -> 1; 0.025 -> 0.5 (the empirical mean)
      case "lesk" => 39.0 * lesk / (38.0 * lesk + 1.0)
      // ^ as above; same mean
      case "elesk" => 39.0 * eLesk / (38.0 * eLesk + 1.0)

      case "cos" => (1.0 + cos) / 2.0
      case "angle" => 1.0 - angle / scala.math.Pi
      // ^ as above; empirical mean at 0.98
      case "jensenshannon" => {
        val x:Double = 1.0 - jensenShannon
        (0.0204 * x) / ( -0.9796 * x + 1 )
      }
      // ^ as above; empirical mean at 0.94
      case "hellinger" => {
        val x:Double = 1.0 - hellinger
        (0.0638 * x) / ( -0.93617 * x + 1 )
      }
      case "jaccard" => jaccard
      // ^ as above; empirical mean at 0.93
      case "dice" => (0.07527 * dice) / ( -0.9247 * dice + 1 )

      case _ => throw new IllegalArgumentException("No such algorithm: " + algorithm)
    }
    if (result.isNaN || result.isInfinite) {
      0.0
    } else {
      result
    }
  }

  /** @see normalizedSimilarity */
  def normalizedSimilarity(algorithm:Symbol):Double = normalizedSimilarity(algorithm.toString)

  /**
   * 1 - normalizedSimilarity
   * @see normalizedSimilarity
   */
  def normalizedDistance(algorithm:String):Double
    = 1.0 - normalizedSimilarity(algorithm)
  
  /** @see normalizedDistance */
  def normalizedDistance(algorithm:Symbol):Double = normalizedDistance(algorithm.toString)
  

  private lazy val bilinearTransformsGaussian:Map[String,Double=>Double] = Map(
    "path" ->          bilinearBinaryTransform(-3.299, -2.479, -1.660),
    "resnik" ->        bilinearBinaryTransform(0.0, 1.055, 3.805),
    "lin" ->           bilinearBinaryTransform(0.0, 0.064, 0.231),
    "jc" ->            bilinearBinaryTransform(-0.007, 0.034, 0.075),
    "wupalmer" ->      bilinearBinaryTransform(0.0, 0.214, 1.0), // bounded [0, 1]
    "lesk" ->          bilinearBinaryTransform(0.0, 0.029, 1.0),  // bounded [0,1]
    "elesk" ->         bilinearBinaryTransform(0.0, 0.029, 1.0),  // copied from Lesk

    "cos" ->           bilinearBinaryTransform( -1.0, 0.232, 1.0),
    "angle" ->         bilinearBinaryTransform( 0.0, 1.247, scala.math.Pi),
    "jensenshannon" -> bilinearBinaryTransform(0.0, 0.019, 1.0 ),
    "hellinger" ->     bilinearBinaryTransform(0.0, 0.059, 1.0 ),
    "jaccard" ->       bilinearBinaryTransform(0.0, 0.877, 1.0 ),
    "dice" ->          bilinearBinaryTransform(0.0, 0.933, 1.0)
  )

  private lazy val bilinearTransformsPercentile:Map[String,Double=>Double] = Map(
    "path" ->          bilinearBinaryTransform(-3.434, -2.565, 0),
    "resnik" ->        bilinearBinaryTransform(0.0, 0.676, 3.805),  // max doesn't quite make sense
    "lin" ->           bilinearBinaryTransform(0, 0.039, 1.0),
    "jc" ->            bilinearBinaryTransform(0.017, 0.032, 16.96),
    "wupalmer" ->      bilinearBinaryTransform(0.0, 0.154, 1.0),
    "lesk" ->          bilinearBinaryTransform(0.0, 0.016, 1.0),
    "elesk" ->         bilinearBinaryTransform(0.0, 0.016, 1.0),  // copied from Lesk

    "cos" ->           bilinearBinaryTransform( -1.0, 0.101, 1.0),
    "angle" ->         bilinearBinaryTransform( 0.0, 1.469, scala.math.Pi),
    "jensenshannon" -> bilinearBinaryTransform(0.0, 0.018, 1.0 ),
    "hellinger" ->     bilinearBinaryTransform(0.0, 0.068, 1.0 ),
    "jaccard" ->       bilinearBinaryTransform(0.0, 0.857, 1.0 ),
    "dice" ->          bilinearBinaryTransform(0.0, 0.923, 1.0)
  )

  /**
   * A normalized similarity measure; taking the original similarity value
   * and passing it thorugh a bilinear (i.e., linear fractional, mobius)
   * transformation to constrict it to a [0,1] range.
   * 
   * @param algorithm The algorithm to use
   * @return A distance, between 0 and 1.
   *         1 means identical; 0 means polar opposite
   */
  def bilinearBinarySimilarity(algorithm:String):Double = {
    val transform = bilinearTransformsPercentile(algorithm.toLowerCase)
    val transformed = algorithm.toLowerCase match {
      case "path" => transform(path)
      case "resnik" => transform(resnik)
      case "lin" => transform(lin)
      case "jc" => transform(jc)
      case "wupalmer" => transform(wuPalmer)
      case "lesk" => transform(lesk)
      case "elesk" => transform(eLesk)

      case "cos" => transform(cos)
      case "angle" => 1.0 - transform(angle)
      case "jensenshannon" => 1.0 - transform(jensenShannon)
      case "hellinger" => 1.0 - transform(hellinger)
      case "jaccard" => transform(jaccard)
      case "dice" => transform(dice)

      case _ => throw new IllegalArgumentException("No such algorithm: " + algorithm)
    }
    scala.math.min(1.0, scala.math.max(0.0, transformed))
  }
  
  /** @see bilinearBinarySimilarity */
  def bilinearBinarySimilarity(algorithm:Symbol):Double = bilinearBinarySimilarity(algorithm.toString)

  /**
   * Apply the given algorithm to assess similarity.
   * @param algorithm The algorithm to use
   * @return The similarity returned by the algorithm
  */
  def apply(algorithm:String):Double = {
    val result = algorithm.toLowerCase match {
      case "path" => path
      case "resnik" => resnik
      case "lin" => lin
      case "jc" => jc
      case "wupalmer" => wuPalmer
      case "lesk" => lesk
      case "elesk" => eLesk

      case "cos" => cos
      case "angle" => angle
      case "jensenshannon" => jensenShannon
      case "hellinger" => hellinger
      case "jaccard" => jaccard
      case "dice" => dice

      case _ => throw new IllegalArgumentException("No such algorithm: " + algorithm)
    }
    result
  }

  /** @see apply */
  def apply(algorithm:Symbol):Double = apply(algorithm.toString)
}

object Similarity {
  import Util._
  var cache = new SynchronizedQueue[((Sentence,Sentence),Similarity)]()
  
  val algorithms = Set[String](
      "path",
      "resnik",
      "lin",
      "jc",
      "wupalmer",
      "lesk",
//      "eLesk",
      "cos",
      "angle",
      "jensenshannon",
      "hellinger",
      "jaccard",
      "dice"
    )

  def apply(sentA:Sentence, sentB:Sentence, backoff:Boolean)
           (implicit wordnet:Ontology, distsim:DistSim):Similarity = {
    val cacheHits = cache.filter( _._1 == (sentA, sentB) )
    if (cacheHits.size > 0) return cacheHits(0)._2
    val rtn = new Similarity(filterNonsense(sentA),
                             filterNonsense(sentB),
                             backoff, wordnet, distsim)
    if (cache.size >= 25) cache.dequeue
    cache.enqueue( ((sentA, sentB), rtn) )
    rtn
  }
  
  def apply(sentA:Sentence, sentB:Sentence)
           (implicit wordnet:Ontology, distsim:DistSim):Similarity
    = apply(sentA, sentB, true)

  def apply(a:String, b:String)
           (implicit wordnet:Ontology, distsim:DistSim):Similarity
    = apply(Sentence(a.split("""\s+""")), Sentence(b.split("""\s+""")))
  
  def apply(a:Seq[String], b:Seq[String])
           (implicit wordnet:Ontology, distsim:DistSim):Similarity
    = apply(new Sentence(a.toArray), new Sentence(b.toArray))

  def apply(sentA:Sentence, sentB:Option[Sentence])
    (implicit wordnet:Ontology, distsim:DistSim):Similarity = {
    assert(sentB.isDefined)
    apply(sentA, sentB.get)
  }


  /**
   * To run me, you have to have the wordnet directory passed as an environment variable,
   * "wordnet.database.dir" such as:
   *
   * -Dwordnet.database.dir=/path/to/wordnet/WordNet-3.1/dict
   *
   * And, of course, ontology.ser.gz and distsim.ser.gz have to be at the locations defined
   * in the code below.
   *
   * @param args
   */
  def main(args:Array[String]):Unit = {
    val paths:(String,String) = if (args.length == 2) {
      (args(0), args(1))
    } else {
      ( "/home/gabor/workspace/concept/aux/bare_models/ontology.ser.gz","/home/gabor/workspace/concept/aux/bare_models/distsim.ser.gz" )
    }
    implicit val ontology = Ontology.load(paths._1)
    implicit val distsim = DistSim.load(paths._2)
    println(Similarity("cat", "dog").cos)
  }
}

//------------------------------------------------------------------------------
// Math
//------------------------------------------------------------------------------
object FancyMath {
  import breeze.linalg._

  /**
   * A bilinear (linear fractional; mobius) transform is defined as a transform
   * from x to y such that: (ax + b) / (cx + d) = y
   * @param domainMapping: A map of three points defining the three fixed
   *                       points of the mapping (e.g., the lower, upper, and
   *                       middle of the range)
   * @return A function, such that f(x) = y according to the definition above.
  */
  def bilinearTransform(domainMapping:Map[Double,Double]):Double=>Double = {
    import breeze.linalg.MatrixSingularException;
    // -- Solve Equations
    if (domainMapping.size != 3) { throw new RuntimeException("A bilinear transformation is defined by exactly 3 points") }
    // (tensors)
    val A = DenseMatrix.zeros[Double](3, 3)
    val B = DenseVector.zeros[Double](3)
    // (get coefficients)
    val (a,b,c,d) = {
      try {
        // (case: fix 'd')
        domainMapping.zipWithIndex.foreach{ case ((x:Double, y:Double), i:Int) =>
          A(i, 0) = x
          A(i, 1) = 1.0
          A(i, 2) = -x * y
          B(i) = 1.0 * y
        }
        val coeffs:DenseVector[Double] = A \ B  // solve
        (coeffs(0), coeffs(1), coeffs(2), 1.0)
      } catch { case (e:MatrixSingularException) => try {
        // (case: fix 'b')
        domainMapping.zipWithIndex.foreach{ case ((x:Double, y:Double), i:Int) =>
          A(i, 0) = -x
          A(i, 1) = x * y
          A(i, 2) = y
          B(i) = 1.0
        }
        val coeffs:DenseVector[Double] = A \ B  // solve
        (coeffs(0), 1.0, coeffs(1), coeffs(2))
      } catch { case (e:MatrixSingularException) => try {
        // (case: fix 'c')
        domainMapping.zipWithIndex.foreach{ case ((x:Double, y:Double), i:Int) =>
          A(i, 0) = x
          A(i, 1) = 1.0
          A(i, 2) = -y
          B(i) = x * y
        }
        val coeffs:DenseVector[Double] = A \ B  // solve
        (coeffs(0), coeffs(1), 1.0, coeffs(2))
      } catch { case (e:MatrixSingularException) => try {
        // (case: fix 'a')
        domainMapping.zipWithIndex.foreach{ case ((x:Double, y:Double), i:Int) =>
          A(i, 0) = -1.0
          A(i, 1) = x * y
          A(i, 2) = y
          B(i) = x
        }
        val coeffs:DenseVector[Double] = A \ B  // solve
        (1.0, coeffs(0), coeffs(1), coeffs(2))
      } } } }
    }
    // -- Return Transform
    (x:Double) => {
      if (x == Double.PositiveInfinity) Double.PositiveInfinity
      else if (x == Double.NegativeInfinity) Double.NegativeInfinity
      else (a*x + b) / (c*x + d)
    }
  }
  
  def bilinearBinaryTransform(zeroPoint:Double, midPoint:Double,
                              highPoint:Double):Double=>Double = {
    bilinearTransform(Map[Double,Double](zeroPoint -> 0.0,
                                         midPoint  -> 0.5,
                                         highPoint -> 1.0))
  }

  /**
   * Returns the P-value of normaldist(x).
   * Taken from: http://blade.nagaokaut.ac.jp/~sinara/ruby/scala.math.statistics2/statistics2-0.53/README
   * and, in particular: http://stackoverflow.com/questions/6116770/whats-the-equivalent-of-rubys-pnormaldist-statistics-function-in-haskell
   */
  def pnormdist(qn:Double):Double = {
    val b = Array(1.570796288, 0.03706987906, -0.8364353589e-3,
                  -0.2250947176e-3, 0.6841218299e-5, 0.5824238515e-5,
                  -0.104527497e-5, 0.8360937017e-7, -0.3231081277e-8,
                  0.3657763036e-10, 0.6936233982e-12)
    if (qn <= 0.0 || qn >= 1.0) throw new IllegalArgumentException("out of bounds")
    if (qn == 0.5) return 0.0
    val x = if(qn > 0.5) 1.0 - qn else qn
    val w3 = -scala.math.log(4.0 * x * (1.0 - x))
    var w1 = (1 to 10).foldLeft(b(0)){ case (soFar:Double, i:Int) =>
      soFar + b(i) * scala.math.pow(w3, i.toDouble)
    }
    if (qn > 0.5) scala.math.sqrt(w1 * w3) else -scala.math.sqrt(w1 * w3)
  }

  def wilsonBound(positive:Double, total:Double, confidence:Double, bound:(Double,Double)=>Double = _ - _):Double = {
    if (total == 0) return 0.0
    val z:Double = pnormdist(1.0 - (1.0 - confidence) / 2.0)
    val phat:Double = 1.0 * positive / total
    bound(phat + z*z/(2.0*total), z * scala.math.sqrt((phat*(1.0-phat)+z*z/(4.0*total))/total))/(1.0+z*z/total)
  }

  def wilsonLowerBound(positive:Double, total:Double, confidence:Double):Double
    = wilsonBound(positive, total, confidence, {_ - _})

  def wilsonLowerBound(bernoulliSamples:Seq[Boolean], confidence:Double):Double
    = wilsonLowerBound(bernoulliSamples.filter(_ == true).size, bernoulliSamples.length, confidence)
  
  def wilsonUpperBound(positive:Double, total:Double, confidence:Double):Double
    = wilsonBound(positive, total, confidence, _ + _)

  def wilsonUpperBound(bernoulliSamples:Seq[Boolean], confidence:Double):Double
    = wilsonUpperBound(bernoulliSamples.filter(_ == true).size, bernoulliSamples.length, confidence)
}


//------------------------------------------------------------------------------
// Closed Class Words
//------------------------------------------------------------------------------
object ClosedClassWords {
  def apply(strRaw:String):Boolean = {
    val str = strRaw.toLowerCase
    pronouns(str) || determiners(str) || bes(str) || prepositions(str)
  }

  val pronouns:Set[String] = Set[String](
  	"all",
  	"any",
  	"he",
  	"her",
  	"herself",
  	"him",
  	"himself",
  	"his",
  	"I",
  	"it",
  	"its",
  	"itself",
  	"me",
  	"most",
  	"my",
  	"myself",
  	"none",
  	"our",
  	"ourselves",
  	"she",
  	"some",
  	"that",
  	"their",
  	"them",
  	"themselves",
  	"they",
  	"this",
  	"us",
  	"we",
  	"what",
  	"which",
  	"who",
  	"whom",
  	"whose",
  	"you",
  	"your",
  	"yourself",
  	"yourselves"
  ).map( _.toLowerCase )
  
  val determiners:Set[String] = Set[String](
  	"'s",
  	"a",
  	"an",
  	"another",
  	"any",
  	"certain",
  	"each",
  	"every",
  	"her",
  	"his",
  	"its",
  	"my",
  	"our",
  	"some",
  	"that",
  	"the",
  	"their",
  	"this",
    "someone",
    "everyone"
  ).map( _.toLowerCase )
  
  val determinerAnimacy:Map[String,String] = Map[String,String](
  	"'s"         -> "animal",
  	"a"          -> "object",
  	"an"         -> "object",
  	"another"    -> "object",
  	"any"        -> "object",
  	"certain"    -> "object",
  	"each"       -> "object",
  	"every"      -> "object",
  	"her"        -> "girl",
  	"his"        -> "boy",
  	"its"        -> "object",
  	"my"         -> "person",
  	"our"        -> "people",
  	"some"       -> "object",
  	"that"       -> "object",
  	"the"        -> "object",
  	"their"      -> "people",
  	"this"       -> "object",
  	"those"      -> "object",
  	"these"      -> "object",
    "someone"    -> "person",
    "everyone"   -> "people",
    "anyone"     -> "person",
    "misc"       -> "object",
    "everything" -> "object",
    "something"  -> "object"
  )
  
  val bes:Set[String] = Set[String](
    "am",
    "is",
    "are",
    "was",
    "were",
    "be",
    "being",
    "been"
  ).map( _.toLowerCase )
    
  val prepositions:Set[String] = Set[String](
    "aboard",
  	"about",
  	"above",
  	"across",
  	"after",
  	"against",
  	"along",
  	"amid",
  	"among",
  	"anti",
  	"around",
  	"as",
  	"at",
  	"before",
  	"behind",
  	"below",
  	"beneath",
  	"beside",
  	"besides",
  	"between",
  	"beyond",
  	"but",
  	"by",
  	"concerning",
  	"considering",
  	"despite",
  	"down",
  	"during",
  	"except",
  	"excepting",
  	"excluding",
  	"following",
  	"for",
  	"from",
  	"in",
  	"inside",
  	"into",
  	"like",
  	"minus",
  	"near",
  	"of",
  	"off",
  	"on",
  	"onto",
  	"opposite",
  	"outside",
  	"over",
  	"past",
  	"per",
  	"plus",
  	"regarding",
  	"round",
  	"save",
  	"since",
  	"than",
  	"through",
  	"to",
  	"toward",
  	"towards",
  	"under",
  	"underneath",
  	"unlike",
  	"until",
  	"up",
  	"upon",
  	"versus",
  	"via",
  	"with",
  	"within",
  	"without"
  ).map( _.toLowerCase )
}
