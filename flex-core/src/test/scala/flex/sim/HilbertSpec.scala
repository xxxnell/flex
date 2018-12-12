package flex.sim

import flex.pdf.NumericDist
import flex.measure.syntax._

import org.specs2.ScalaCheck
import org.specs2.mutable._

class HilbertSpec extends Specification with ScalaCheck {

  "Hilbert" should {

    "normForSamplingDist" in {
      val dist = NumericDist.normal(0.0, 1)
      val expected = math.sqrt(1 / (2 * math.sqrt(math.Pi)))
      val calc = Hilbert.norm(dist)
      val cond1 = calc ~= expected

      if(!cond1) ko(s"Norm of std. calculated: $calc, expected: $expected")
      else ok
    }

  }

}