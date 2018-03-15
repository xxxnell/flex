package flip.range

import org.specs2.mutable._
import org.specs2.ScalaCheck

class RangePSpec extends Specification with ScalaCheck {

  "RangeP" should {

    "length" in {
      RangeP(0, 10).length must equalTo(10)
    }

    "intersection" in {
      RangeP.intersection(RangeP(0, 10), RangeP(5, 15)) must equalTo(RangeP(5, 10))
    }

    "overlapPercent" in {

      "basic" in {
        RangeP(0, 10).overlapPercent(RangeP(5, 10)) must equalTo(0.5)
      }

      "if target is sticked out" in {
        RangeP(0, 10).overlapPercent(RangeP(5, 15)) must equalTo(0.5)
      }

    }

    "floorMiddle" in {

      "inf" in {
        val p1 = Double.NegativeInfinity
        val p2 = 10
        val res = RangeP(p1, p2).cutoffMiddle

        if(res.isNaN) ko("NaN")
        else if(res.isInfinity) ko("Infinity")
        else ok
      }

    }

  }

}