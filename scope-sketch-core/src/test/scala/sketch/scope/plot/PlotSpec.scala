package sketch.scope.plot

import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.measure.syntax._
import sketch.scope.range.RangeP

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

    "integral" in {
      val records = (RangeP.point(0), 0d) :: (RangeP.point(1), 1d) :: (RangeP.point(2), 0d) :: Nil
      val plot = CountPlot.disjoint(records)
      val integ = plot.integral(0, 2)

      if(integ != 1) ko(s"Integration result: $integ, expected: 1") else ok
    }

  }

}