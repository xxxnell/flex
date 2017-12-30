package sketch.scope.plot

import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.measure.syntax._

/**
  * Licensed by Probe Technology, Inc.
  */
class PlotSpec extends Specification with ScalaCheck {

  "Plot" should {

    "linearFitting" in {

      "basic" in {
        val res = Plot.linearFitting((1, 1), (2, 2), 1.5)
        val cond1 = res ~= 1.5d

        if(cond1) ok else ko(s"result: $res != 1.5")
      }

      "vertical" in {
        val res = Plot.linearFitting((1, 1), (1, 2), 1)
        val cond1 = res ~= 1.5d

        if(cond1) ok else ko(s"result: $res != 1.5")
      }

    }

  }

}