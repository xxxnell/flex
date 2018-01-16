package flip.plot

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.measure.syntax._
import flip.range.RangeP

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

      "basic" in {
        val records = (RangeP.point(0), 0d) :: (RangeP.point(1), 1d) :: (RangeP.point(2), 0d) :: Nil
        val plot = CountPlot.disjoint(records)
        val integ = plot.integral(0, 2)

        if(integ != 1) ko(s"Integration result: $integ, expected: 1") else ok
      }

      "range out of definition" in {
        val records = (RangeP.point(0), 0d) :: (RangeP.point(1), 1d) :: (RangeP.point(2), 0d) :: Nil
        val plot = CountPlot.disjoint(records)
        val integ = plot.integral(-10, 10)

        if(!(integ ~= 1)) ko(s"Integration result: $integ, expected: 1") else ok
      }

      "tiny 1" in {
        val records = (RangeP.point(0), 0d) :: (RangeP.point(1), 1d) :: (RangeP.point(2), 0d) :: Nil
        val plot = CountPlot.disjoint(records)
        val delta = 1e-2
        val integ = plot.integral(1, 1 + delta)

        if(!(integ ~= delta)) ko(s"Integration result: $integ, expected: $delta") else ok
      }

      "tiny 2" in {
        val records = (RangeP.point(0), 0d) :: (RangeP.point(1), 1d) :: (RangeP.point(2), 0d) :: Nil
        val plot = CountPlot.disjoint(records)
        val delta = 1e-2
        val integ = plot.integral(1.5, 1.5 + delta)

        if(!(integ ~= (delta / 2))) ko(s"Integration result: $integ, expected: ${delta / 2}") else ok
      }

    }

    "planarized" in todo

    "interpolation" in {

      "basic" in {
        val size = 200
        val records = (0.0 until size.toDouble by 1.0).toList.map(p => (RangeP.point(p), math.cos(p)))
        val plot = CountPlot.disjoint(records)
        val interp = plot.interpolation(size/2)

        if(interp.isNaN) ko("NaN")
        else if(interp.isInfinity) ko(s"$interp")
        else ok
      }

      "contains Double.MinValue value" in {
        val records = (RangeP.point(0), Double.MinValue) :: (RangeP.point(0.25), -0.1) :: Nil
        val plot = CountPlot.disjoint(records)
        val interp = plot.interpolation(0.125)
        val expected = Double.MinValue / 2

        if(interp.isNaN) ko("NaN")
        else if(interp.isInfinity) ko(s"$interp")
        else if(!(interp ~= expected)) ko(s"interpolated value ($interp) != expected ($expected)")
        else ok
      }

    }

  }

}