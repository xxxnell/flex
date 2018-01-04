package sketch.scope.range

import org.specs2.mutable._
import org.specs2.ScalaCheck

class RangePSpec extends Specification with ScalaCheck {

  "RangeP" should {

    "length" in todo

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

  }

}