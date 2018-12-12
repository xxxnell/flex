package flex.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flex.cmap.DividerCmapGen.dividerGen

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

      "empty" in {
        Cmap.divider(Nil).size must equalTo(1)
      }

      "basic" in {
        val divider = (1d to 10d by 1d).toList
        Cmap.divider(divider).size must equalTo(divider.size + 1)
      }

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