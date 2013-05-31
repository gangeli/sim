package org.goobs.sim

//(scala)
import scala.collection.JavaConversions._
//(java)
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.util.zip.GZIPOutputStream
//(lib)
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.Redwood
import edu.stanford.nlp._
//(breeze)
import breeze.linalg._
//(project)
import DistSim._


@SerialVersionUID(1l)
case class DistSim(vectors:Map[String,WordVector]) {
  import DistSim._

  lazy val unk = vectors("uuunkkk")
  
  def sim(a:WordVector, b:WordVector):Option[Similarity]
    = if (a == unk && b == unk) None else Some(Similarity(a, b))
  
  def sim(a:String, b:String):Option[Similarity] = sim(apply(a), apply(b))
  
  def sim(a:Seq[String], b:Seq[String]):Option[Similarity] = sim(apply(a), apply(b))

  def sim(a:Sentence, b:Sentence, backoff:Boolean=true):Option[Similarity] = {
    if (a.length == 0 || b.length == 0) return None
    if (backoff) {
      // -- Case: Massage Sentences
      sim(Util.massage(a, contains(_)).getOrElse(a.word.toList),
          Util.massage(b, contains(_)).getOrElse(b.word.toList))
    } else {
      // -- Case: Exact Match
      sim(a.words, b.words)
    }
  }

  def contains(word:String):Boolean = vectors.contains(word.toLowerCase)
  
  def contains(phrase:Seq[String]):Boolean
    = phrase.forall( w => vectors.contains(w.toLowerCase) )
  
  def isUnk(phrase:String):Boolean = !contains(phrase)
  def isUnk(phrase:Seq[String]):Boolean
    = phrase.exists( w => vectors.contains(w.toLowerCase) )

  def apply(word:String):WordVector = vectors.get(word) match {
    case Some(vector) => vector
    case None => vectors.get(word.toLowerCase) match {
      case Some(vector) => vector
      case None => unk
    }
  }
  
  def apply(phrase:Seq[String]):WordVector = {
    if (phrase.length == 0) apply("")
    else if (phrase.forall( isUnk(_) )) unk
    else phrase.tail.map( (s:String) => apply(s) )
        .foldLeft(apply(phrase.head)){
            case (sum:DenseVector[Double], vector:DenseVector[Double]) =>
          sum + vector
        } / phrase.length.toDouble
  }
  
  def save(filename:String):Unit = {
    startTrack("Serializing vectors")
    // (writing file)
    val outStream = new ObjectOutputStream(
                        new GZIPOutputStream(new FileOutputStream(filename)))
    Redwood.log("writing file " + filename)
    outStream.writeObject(this)
    outStream.close
    endTrack("Serializing vectors")
  }
  
}

@SerialVersionUID(1l)
object DistSim {
  type WordVector = DenseVector[Double]
 
  def load(filename:String):DistSim = Util.readObject(filename,
                                                      Some("Loading DistSim"))

  @SerialVersionUID(2l)
  case class Similarity(a:WordVector, b:WordVector) {
    private lazy val sigmoidA
      = a.map{ x => 1.0 / (1.0 + scala.math.exp(-x)) }
    private lazy val sigmoidB
      = b.map{ x => 1.0 / (1.0 + scala.math.exp(-x)) }

    private lazy val (klAB, klBA):(Double,Double) = {
      val sumA = sigmoidA.sum
      val sumB = sigmoidB.sum
      assert(sumA >= 0 && sumA != Double.PositiveInfinity && !sumA.isNaN)
      assert(sumB >= 0 && sumB != Double.PositiveInfinity && !sumB.isNaN)
      (
        (0 until a.size).foldLeft(0.0){ case (sum:Double, i:Int) =>
          assert(sigmoidA(i) >= 0.0, "Not a distribution: " + sigmoidA)
          sum + (sigmoidA(i) / sumA) *
            (scala.math.log(sigmoidA(i) / sumA) - scala.math.log(sigmoidB(i) / sumB))
        },
        (0 until a.size).foldLeft(0.0){ case (sum:Double, i:Int) =>
          assert(sigmoidB(i) >= 0.0, "Not a distribution: " + sigmoidB)
          sum + (sigmoidB(i) / sumB) *
            (scala.math.log(sigmoidB(i) / sumB) - scala.math.log(sigmoidA(i) / sumA))
        }
      )
    }

    lazy val cos:Double = {
      val denom:Double = (norm(a, 2) * norm(b, 2))
      val numer:DenseVector[Double] = a.t * b
      assert(numer.length == 1)
      scala.math.min(1.0, scala.math.max(numer(0) / denom, -1.0))
    }

    lazy val angle:Double = {
      var angle:Double = scala.math.acos(cos)
      assert(angle >= 0.0 && angle <= scala.math.Pi, "Bad angle: " + angle)
      angle
    }

    lazy val jensenShannon:Double = (klAB + klBA) / 2.0;

    lazy val hellinger:Double = {
      val sumA = sigmoidA.sum
      val sumB = sigmoidB.sum
      val sum = (0 until a.size).foldLeft(0.0){ case (sum:Double, i:Int) =>
        val term = scala.math.sqrt(sigmoidA(i) / sumA) - scala.math.sqrt(sigmoidB(i) / sumB)
        sum + (term * term)
      }
      (1 / scala.math.sqrt(2.0)) * scala.math.sqrt(sum);
    }

    lazy val jaccard:Double = {
      val (min,max) = (0 until a.size).foldLeft((0.0,0.0)){ 
          case ((min:Double, max:Double), i:Int) =>
        (min + scala.math.min(sigmoidA(i), sigmoidB(i)),
         max + scala.math.max(sigmoidA(i), sigmoidB(i)))
      }
      min / max
    }
    
    lazy val dice:Double = {
      val (min,sum) = (0 until a.size).foldLeft((0.0,0.0)){ 
          case ((min:Double, sum:Double), i:Int) =>
        (min + scala.math.min(sigmoidA(i), sigmoidB(i)),
         sum + sigmoidA(i) + sigmoidB(i))
      }
      2.0 * min / sum
    }
  }
}
