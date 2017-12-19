package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.cmap.DividerCmapGen.dividerGen
//import sketch.scope.cmap.Cmap

/**
  * Licensed by Probe Technology, Inc.
  */
class CmapSpec extends Specification with ScalaCheck {

  "Cmap" should {

    "bin" in {

      "divider" in {
//        implicit val dividerGen: Arbitrary[List[Double]] = CmapGen.dividerA
//
//        prop { (divider: List[Double]) =>
//          val cmap = DividerCmap(divider)
//          val dividerSize = divider.size
//          val check = (1 until dividerSize)
//            .map(idx => (idx, cmap.bin(idx)))
//            .forall { case (idx, bin) =>
//              bin.start == divider(idx - 1) && bin.end == divider(idx)
//            }
//          if(check) ok else ko
//        }.setArbitrary(dividerGen)

        todo
      }

      "uniform" in {
        todo
      }

    }

    "size" in {
      implicit val cmapGen: Arbitrary[(Int, Cmap)] = CmapGen.cmapA

      prop { (sizeCmap: (Int, Cmap) ) =>
        val (size, cmap) = sizeCmap
        if(size == cmap.size) ok else ko
      }.setArbitrary(cmapGen)
    }

    "range" in {

      "divider" in {
//        implicit val dividerGen: Arbitrary[List[Double]] = CmapGen.dividerA
//
//        prop { (divider: List[Double]) =>
//          val cmap = DividerCmap(divider)
//          val dividerSize = divider.size
//          val check = (1 until dividerSize)
//            .map(idx => (idx, cmap.range(idx)))
//            .forall { case (idx, range) =>
//              range.start == divider(idx - 1) && range((divider(idx) - divider(idx - 1)).toInt) == divider(idx)
//            }
//          if(check) ok else ko
//        }.setArbitrary(dividerGen)

        todo
      }

      "uniform" in {
        todo
      }

    }

  }

}

object CmapGen {
  
  def cmapGen: Gen[(Int, Cmap)] = UniformCmapGen.uniformCmapGen

  def dividerGen: Gen[List[Double]] = for {
    from <- Gen.choose(0, 100)
    to <- Gen.choose(0, 100)
    if from < to
    list = (from to to).toList.map(a => a.toDouble)
  } yield list

  def cmapA: Arbitrary[(Int, Cmap)] = Arbitrary(cmapGen)

  def dividerA: Arbitrary[List[Double]] = Arbitrary(dividerGen)

}