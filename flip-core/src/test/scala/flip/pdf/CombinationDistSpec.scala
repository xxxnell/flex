package flip.pdf

import flip.implicits._
import org.specs2.mutable._
import org.specs2.ScalaCheck

class CombinationDistSpec extends Specification with ScalaCheck {

  "ConbinationDistSpec" should {

    "icdf" in {
      val bimodal = { (0.5, NumericDist.normal(-2.0, 1)) + (0.5, NumericDist.normal(2.0, 1)) }
      val p = bimodal.icdf(0.95)

      if(p.isNaN) ko(s"icdf(0.95): $p")
      else if(p.isInfinity) ko(s"icdf(0.95): $p")
      else ok
    }

  }

}