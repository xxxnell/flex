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

      "ordering" in {
        val p = 0.9945054945054947
        val data =
          (RangeP(0.8800000000000001,	0.8800000000000001),	1.1111111111111107) ::
            (RangeP(0.8800000000000001,	0.8800000000000001), 1.1111111111111107 ) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 2.222222222222223) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 2.222222222222223) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 3.333333333333334) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 4.444444444444445) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 4.444444444444445) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 5.555555555555557) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 5.555555555555557) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 6.666666666666668) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 6.666666666666668) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 7.777777777777779) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 7.777777777777779) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 8.88888888888889) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 8.88888888888889) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 10.0) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 10.0) ::
            (RangeP(1.0000000000000002,	1.0000000000000002), 1.7976931348623157E308) :: Nil
        val interp = DensityPlot.disjoint(data).interpolation(p)

        val cond1 = interp > 1.1111111111111107 && interp < 2.222222222222223
        if(!cond1) ko(s"interpolation of $p is not in expected range. interpolation: $interp")
        else ok
      }

    }

  }

}