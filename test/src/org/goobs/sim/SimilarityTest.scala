package org.goobs.sim

//(scala)
import scala.collection.JavaConversions._
//(scalatest)
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
//(breeze)
import breeze.linalg._
//(program)
import org.goobs.sim._
import org.goobs.sim.Ontology._

object Globals {
  implicit val ontology = Ontology.load("etc/ontology.ser.gz")
  implicit val distsim = DistSim.load("etc/distsim.ser.gz")
}

class WordnetOntologySpec extends FunSpec with ShouldMatchers {
  import Globals._

  describe("An Ontology") {
    // -- Contains
    it("should contain 'Cat'") { ontology.contains("Cat") should be (true) }
    it("should contain 'cat'") { ontology.contains("cat") should be (true) }
    it("should contain 'first council of nicaea'") {
      ontology.contains("first council of nicaea") should be (true)
    }
    it("should contain 'First counCil of nicaea'") {
      ontology.contains("First counCil of nicaea") should be (true)
    }
    it("should contain [First counCil of nicaea]") {
      ontology.contains(
        "First counCil of nicaea".split(" ").toList) should be (true)
    }
    // -- Other Properties
    it("should have consistent depth (i.e., 22)") {
      ontology.depth should be (ontology.computeDepth(ontology.root))
    }
  }

  describe("Ontology element 'Cat'") {
    it("should actually exist") { ontology.get("Cat").size should be > 0 }
    it("should have count > 45926624 (synonomy)") {
      ontology("Cat").count should be > 45926624L
    }
  }
  
  describe("Ontology element 'first council of nicaea'") {
    it("should actually exist") {
      ontology.get("first council of nicaea").size should be > 0
    }
    it("should have count of 3888") {
      ontology("first council of nicaea").count should be === (3888L + 60906L)
    }
  }
}

class WordnetSimilaritySpec extends FunSpec with ShouldMatchers {
  import Globals._
  
  def allLessThan(simA:Ontology.Similarity, simB:Ontology.Similarity):Unit = {
    it("[" + simA + "].path < [" + simB + "].path") { 
      simA.path should be < simB.path
    }
    it("[" + simA + "].resnik < [" + simB + "].resnik") {
      simA.resnik should be < simB.resnik
    }
    it("[" + simA + "].lin < [" + simB + "].lin") {
      simA.lin should be < simB.lin
    }
    it("[" + simA + "].jc < [" + simB + "].jc") {
      simA.jc should be < simB.jc
    }
    it("[" + simA + "].lesk < [" + simB + "].lesk") {
      if (simA.lesk != 0 || simB.lesk != 0) {
        simA.lesk should be < simB.lesk
      }
    }
    it("[" + simA + "].approximateLesk < [" + simB + "].approximateLesk") {
      if (simA.approximateLesk != 0 || simB.approximateLesk != 0) {
        simA.approximateLesk should be < simB.approximateLesk
      }
    }
  }
  
  def allLessThanJaccard(simA:Ontology.Similarity, simB:Ontology.Similarity):Unit = {
    it("[" + simA + "].path < [" + simB + "].path") { 
      simA.aggregateJaccard( _.path ) should be < simB.aggregateJaccard( _.path )
    }
    it("[" + simA + "].resnik < [" + simB + "].resnik") {
      simA.aggregateJaccard( _.resnik ) should be < simB.aggregateJaccard( _.resnik )
    }
    it("[" + simA + "].lin < [" + simB + "].lin") {
      simA.aggregateJaccard( _.lin ) should be < simB.aggregateJaccard( _.lin )
    }
    it("[" + simA + "].jc < [" + simB + "].jc") {
      simA.aggregateJaccard( _.jc ) should be < simB.aggregateJaccard( _.jc )
    }
    it("[" + simA + "].lesk < [" + simB + "].lesk") {
      if (simA.lesk != 0 || simB.lesk != 0) {
        simA.aggregateJaccard( _.lesk ) should be < simB.aggregateJaccard( _.lesk )
      }
    }
    it("[" + simA + "].approximateLesk < [" + simB + "].approximateLesk") {
      if (simA.approximateLesk != 0 || simB.approximateLesk != 0) {
        simA.aggregateJaccard( _.approximateLesk ) should be < simB.aggregateJaccard( _.approximateLesk )
      }
    }
  }
  
  def allEqualTo(simA:Ontology.Similarity, simB:Ontology.Similarity):Unit = {
    it("[" + simA + "].path = [" + simB + "].path") { 
      simA.path should be (simB.path)
    }
    it("[" + simA + "].resnik] - [" + simB + "].resnik") {
      simA.resnik should be (simB.resnik)
    }
    it("[" + simA + "].lin = [" + simB + "].lin") {
      simA.lin should be (simB.lin)
    }
    it("[" + simA + "].jc = [" + simB + "].jc") {
      simA.jc should be (simB.jc)
    }
    it("[" + simA + "].lesk = [" + simB + "].lesk") {
      simA.lesk should be (simB.lesk)
    }
    it("[" + simA + "].approximateLesk = [" + simB + "].approximateLesk") {
      simA.approximateLesk should be (simB.approximateLesk)
    }
    it("[" + simA + "].eLesk = [" + simB + "].eLesk") {
      simA.eLesk should be (simB.eLesk)
    }
  }

  describe("The following inequalities should hold") {
    // (terms to check)
    val dog2cat = ontology.sim("leopard", "cat").get
    val dog2god = ontology.sim("leopard", "god").get
    val dog2water = ontology.sim("leopard", "water").get
    val dog2washington = ontology.sim("leopard", "washington").get
    // (perform checks)
    allLessThan(dog2god, dog2cat)
    allLessThan(dog2water, dog2cat)
    allLessThan(dog2washington, dog2cat)
  }
  
  describe("The following inequalities should hold with jaccard aggregation") {
    // (terms to check)
    val dog2cat = ontology.sim("leopard", "cat").get
    val dog2god = ontology.sim("leopard", "god").get
    val dog2water = ontology.sim("leopard", "water").get
    val dog2washington = ontology.sim("leopard", "washington").get
    // (perform checks)
//    allLessThanJaccard(dog2god, dog2cat)
    allLessThanJaccard(dog2water, dog2cat)
    allLessThanJaccard(dog2washington, dog2cat)
  }
  
  describe("The following equalities should hold") {
    val dog2cat = ontology.sim("dog", "cat").get
    val cat2dog = ontology.sim("cat", "dog").get
    allEqualTo(dog2cat, cat2dog)
  }

  describe("An item with itself") {
    val sim = ontology.sim("Jesus", "Jesus").get
    it("should have path similarity +∞") {
      sim.path should be (Double.PositiveInfinity)
    }
    it("should have Resnik similarity > 0") {
      sim.resnik should be > 0.0
    }
    it("should have Lin similarity 1") {
      sim.lin should be (1.0)
    }
    it("should have JC similarity +∞") {
      sim.jc should be (Double.PositiveInfinity)
    }
    it("should have Wu Palmer similarity 1.0") {
      sim.wuPalmer should be (1.0)
    }
    it("should have Lesk similarity 1") {
      sim.lesk should be (1.0)
    }
    it("should have Approximate Lesk similarity 1") {
      sim.approximateLesk should be (1.0)
    }
    it("should have eLesk similarity < 1") {
      sim.eLesk should be (0.5555555 plusOrMinus 1e-5)
    }
    it("should have reflexive lesk similarity") {
      ontology.sim("frump", "liger").get.lesk should be (
        ontology.sim("liger", "frump").get.lesk)
      ontology.sim("frump", "liger").get.approximateLesk should be (
        ontology.sim("liger", "frump").get.approximateLesk)
    }
    
    val sim2 = ontology.sim("cat", "cat").get
    it("should have explicit max [lin] similarity of 1.0") {
      sim2.aggregateMax(_.lin) should be (1.0)
    }
    it("should have explicit max [jc] similarity of +∞") {
      sim2.aggregateMax(_.jc) should be (Double.PositiveInfinity)
    }
    it("should have explicit aggregateJaccard [lin] similarity of 1.0") {
      sim2.aggregateJaccard(_.lin) should be (1.0)
    }
    it("should have explicit aggregateJaccard [jc] similarity of 1.0") {
      sim2.aggregateJaccard(_.jc) should be (1.0)
    }
    it("should have explicit sigmoided aggregateJaccard [lin] similarity of < 1.0") {
      sim2.aggregateJaccard(_.lin, true) should be < 1.0
    }
  }
  
  describe("The synset n02121620") {
    it("should exist") {
      ontology.id2node.get("n02121620") should be ('defined)
    }
    it("should correspond to 'cat'") {
      ontology.id2node.get("n02121620").get.toString should be ("cat")
    }
  }
  
  describe("Synset similarities") {
    it("should match phrase similarities") {
      val leopard2catPhrase = ontology.sim("leopard", "cat").get
      val leopard2catSynset = ontology.synsetSim("n02128385", "n02121620")
      leopard2catPhrase.path should be (leopard2catSynset.path plusOrMinus 1e-5)
      leopard2catPhrase.resnik should be (leopard2catSynset.resnik plusOrMinus 1e-5)
      leopard2catPhrase.lin should be (leopard2catSynset.lin plusOrMinus 1e-5)
      leopard2catPhrase.jc should be (leopard2catSynset.jc plusOrMinus 1e-5)
      leopard2catPhrase.wuPalmer should be (leopard2catSynset.wuPalmer plusOrMinus 1e-5)
    }
  }
}

class DistributionalSimilaritySpec extends FunSpec with ShouldMatchers {
  import Globals._

  describe("Distributional Similarity") {
    // -- Contains
    it("should contain 'Cat'") { distsim.contains("Cat") should be (true) }
    it("should contain 'cat'") { distsim.contains("cat") should be (true) }
    it("should not crash on 'first council of nicaea'") {
      distsim.contains("first council of nicaea")
    }
    it("should not crash on [First counCil of nicaea]") {
      distsim(
        "First counCil of nicaea".split(" ").toList)
    }
    it("should contain (with unks) [First counCil of nicaea]") {
      (distsim(
        "First counCil of nicaea".split(" ").toList) != (distsim.unk)) should be (true)
    }
  }
}

class HungarianAlgorithmSpec extends FunSpec with ShouldMatchers {
  import scalation.maxima.Hungarian
  describe ("A square matrix of i*j") {
    it ("should find a solution") {
      val cost = DenseMatrix.zeros[Double](100, 100)
      for (i <- 0 until 100; j <- 0 until 100) { cost(i,j) = i * j }
      Hungarian(cost).solve() should be > (-1.0)
    }
    it ("should find the right solution") {
      val cost = DenseMatrix.zeros[Double](100, 100)
      for (i <- 0 until 100; j <- 0 until 100) { cost(i,j) = i * j }
      Hungarian(cost).solve() should be (328350.0 plusOrMinus 0.25)
    }
  }
  describe ("A rectangular matrix of i*j") {
    it ("should find a solution") {
      val cost = DenseMatrix.zeros[Double](10, 100)
      for (i <- 0 until 10; j <- 0 until 100) { cost(i,j) = i * j }
      Hungarian(cost).solve() should be > (-1.0)
    }
    it ("should find the right solution") {
      val cost = DenseMatrix.zeros[Double](10, 100)
      for (i <- 0 until 10; j <- 0 until 100) { cost(i,j) = i * j }
      Hungarian(cost).solve() should be (4335.0 plusOrMinus 0.25)
    }
  }
}

class NormalizedDistanceSpec extends FunSpec with ShouldMatchers {
  import Globals._
  import Util._
   
  private def bound(dist:(Seq[String],Seq[String])=>Double, n:Int = 10
      ):(Double, Double,Double) = {
    val (lower, upper, all) = ontology.ontology.keys.slice(0,n).zip(ontology.ontology.keys.slice(n, 2*n))
           .foldLeft( (Double.PositiveInfinity, Double.NegativeInfinity, List[Double]()) ) {
        case ((lower:Double, upper:Double, all:List[Double]), (a:Phrase, b:Phrase)) =>
      val distVal = dist(a,b)
      if (distVal.isNaN) (lower, upper, all)
      else (math.min(distVal, lower), math.max(distVal, upper), distVal :: all)
    }
    val distribution = all.sortBy( x => x )
    (lower, upper, distribution(n / 2))
  }

  private def enforceBound(dist:(Seq[String],Seq[String])=>Double, n:Int = 100,
                           lb:Double = 0.01, ub:Double = 0.9):Unit = {
    val (lower, upper, median):(Double,Double,Double) = bound(dist, n)
    it("should be bounded below by 0 (via sampling)") { lower should be > -1e-5 }
    it("should be bounded above by 1 (via sampling)") { upper should be < 1.0 + 1e-5 }
    it("should have a median below " + ub) { median should be < ub }
    it("should have a median above " + lb) { median should be > lb }
    it("should be one between a concept and itself") {
      val distVal = dist(List[String]("cat"), List[String]("cat"))
      distVal should be (1.0 plusOrMinus 1e-3)
    }
  }

  private def enforceNormalized(name:String, cap:Int=10000):Unit = {
    describe("The normalized bound for " + name + " similarity") {
      enforceBound(Similarity(_,_).normalizedSimilarity(name), cap)
    }
  }
  enforceNormalized("path")
  enforceNormalized("resnik")
  enforceNormalized("lin")
  enforceNormalized("jc")
  enforceNormalized("wuPalmer")
  enforceNormalized("lesk", 100)
//  enforceNormalized("eLesk") // TODO(gabor) too slow

  enforceNormalized("cos")
  enforceNormalized("angle")
  enforceNormalized("jensenShannon")
  enforceNormalized("hellinger")
  enforceNormalized("jaccard")
  enforceNormalized("dice")
  
  private def enforceBilinear(name:String, cap:Int=10000):Unit = {
    describe("The bilinear bound for " + name + " similarity") {
      enforceBound(Similarity(_,_).bilinearBinarySimilarity(name), cap, 0.45, 0.55)
    }
  }
  enforceBilinear("path")
  enforceBilinear("resnik")
  enforceBilinear("lin")
  enforceBilinear("jc")
  enforceBilinear("wuPalmer")
  enforceBilinear("lesk", 100)
//  enforceBilinear("eLesk") // TODO(gabor) too slow

  enforceBilinear("cos")
  enforceBilinear("angle")
  enforceBilinear("jensenShannon")
  enforceBilinear("hellinger")
  enforceBilinear("jaccard")
  enforceBilinear("dice")
  
}








//
// Gather means and bounds for similarity values
//  
//  import Phrase._
//  val keys = ontology.ontology.keys.toArray
//  val rand = new scala.util.Random(42)
//  List("path",
//       "resnik",
//       "lin",
//       "jc",
//       "wuPalmer",
//       "lesk",
//       "cos",
//       "angle",
//       "jensenShannon",
//       "hellinger",
//       "jaccard",
//       "dice").foreach{ (alg:String) =>
//      val n:Int = 1000000
//      val (sum, sumSquared, count, all) = (0 until n).foldLeft( (0.0, 0.0, 0.0, List[Double]()) ) {
//          case ((sum:Double, sumSquared:Double, count:Double, all:List[Double]), i:Int) =>
//        var distVal = Similarity(keys(rand.nextInt(keys.length)),keys(rand.nextInt(keys.length)))(alg)
//        if (distVal == Double.PositiveInfinity || distVal == Double.NegativeInfinity) {
//          (sum, sumSquared, count, distVal :: all)
//        } else {
//          (sum + distVal, sumSquared + distVal * distVal, count + 1, distVal :: all)
//        }
//      }
//      val distribution = all.toArray.sortBy(x => x)
//      val mean = sum / count
//      val stdev = math.sqrt(sumSquared / count - mean * mean)
//      val median = distribution(n / 2)
//      var i = 0
//      while(distribution(i) == Double.NegativeInfinity) { i += 1 }
//      val lowerBound = distribution(i)
//      i = n-1
//      while(distribution(i) == Double.PositiveInfinity) { i -= 1 }
//      val upperBound = distribution(i)
////      println(alg + ": low= " + (mean - stdev * 2.0) + "  mid= " + mean + "  high=" + (mean + stdev*2.0))
//      println(alg + ": low= " + lowerBound + "  mid= " + median + "  high=" + upperBound)
//   }
