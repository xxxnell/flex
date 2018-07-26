package flip.plot

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.measure.syntax._

class FittingSpec extends Specification with ScalaCheck {

  "Fitting" should {

    "linearFitting" in {

      "basic" in {
        val resO = Fitting.linearFitting((1, 1), (2, 2), 1.5)
        val cond1 = resO.fold(false)(_ ~= 1.5d)

        if(cond1) ok else ko(s"result: $resO != 1.5")
      }

      "vertical" in {
        val resO = Fitting.linearFitting((1, 1), (1, 2), 1)
        val cond1 = resO.fold(false)(_ ~= 1.5d)

        if(cond1) ok else ko(s"result: $resO != 1.5")
      }

    }

  }

}