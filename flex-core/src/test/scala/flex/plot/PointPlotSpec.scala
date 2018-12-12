package flex.plot

import org.specs2.mutable._
import org.specs2.ScalaCheck
import cats.data.NonEmptyList
import flex.measure.syntax._
import flex.pdf.NormalDist

class PointPlotSpec extends Specification with ScalaCheck {

  "PointPlot" should {

    "constructor" in {

      "deltas" in {

        "empty" in {
          val plot = PointPlot.deltas(Nil, 1E-10)
          val cond1 = plot.records.length == 0

          if(!cond1) ko(plot.toString)
          else ok
        }

        "overlapped" in {
          val ds = (0.0, 1.0) :: (2.0, 1.0) :: (1.0, 1.0) :: (0.0, 1.0) :: Nil
          val plot = PointPlot.deltas(ds, 1E-10)
          val cond1 = plot.records.toList.sliding(2).forall {
            case (x1, _) :: (x2, _) :: Nil => x1 < x2
          }
          val cond2 = PointPlot.integralAll(plot) ~= 4
          val cond3 = plot.of(0) > 0 && plot.of(1) > 0 && plot.of(2) > 0

          if(!cond1) ko(s"x of records: ${plot.records.toList.map(_._1)}")
          else if(!cond2) ko(s"counts: ${PointPlot.integralAll(plot)}")
          else if(!cond3) ko(s"pdf(0): ${plot.of(0)}, pdf(1): ${plot.of(1)}, pdf(2): ${plot.of(2)}")
          else ok
        }

      }

    }

    "interpolation" in {

      "basic" in {
        val f = (i: Double) => math.pow(math.cos(i), 2)
        val domains = (1 to 10).map(_.toDouble).toArray
        val records1 = domains.map(i => (i, math.pow(math.cos(i), 2)))
        val plot1 = PointPlot(records1)

        val cond1 = plot1.interpolation(2.5) ~= ((f(2) + f(3)) / 2)

        if(!cond1) ko(s"observed cos^2(1.5) == ${plot1.interpolation(2.5)}, expected: ${(f(2) + f(3)) / 2}")
        else ok
      }

      "overlapped index" in {
        val records = 0.9807692307688678 -> 5.000005 ::
          0.9807692307688678 -> 5.625 ::
          1.0 -> 6.875 ::
          1.0 -> 6.875 :: Nil
        val plot1 = PointPlot(records.toArray)
        val intp = plot1.interpolation(0.9878048780487806)

        val cond1 = intp > 5.625 && intp < 6.875
        if(!cond1) ko(s"$intp")
        else ok
      }

      "out of scope 1" in {
        val (v1, v2) = (1.0, 2.0)
        val records =
          0.0 -> v1 ::
            1.0 -> 2.0 ::
            2.0 -> 3.0 ::
            3.0 -> v2 :: Nil
        val plot1 = PointPlot(records.toArray)
        val cond1 = plot1.interpolation(-10.0) == v1
        val cond2 = plot1.interpolation(10.0) == v2

        if(!cond1) ko(s"${plot1.interpolation(-10.0)} != $v1")
        if(!cond2) ko(s"${plot1.interpolation(10.0)} != $v2")
        else ok
      }

      "out of scope 2" in {
        val v1 = -944.4444444444445
        val x = -0.05
        val records =
          0.01 -> v1 ::
            0.01 -> -833.3333333333334 ::
            0.01 -> -722.2222222222222 :: Nil
        val plot1 = PointPlot(records.toArray)
        val cond1 = plot1.interpolation(x) == v1

        if(!cond1) ko(s"${plot1.interpolation(-10.0)} != $v1")
        else ok
      }

    }

    "add" in {

      "basic" in {
        val domains1 = (1 to 10).map(_.toDouble).toArray
        val domains2 = domains1.map(i => i + 0.1)
        val records1 = domains1.map(i => (i, math.pow(math.cos(i), 2)))
        val records2 = domains2.map(i => (i, math.pow(math.sin(i), 2)))

        val plot1 = PointPlot(records1)
        val plot2 = PointPlot(records2)

        val plot3 = PointPlot.add(NonEmptyList.of((1.0, plot1), (1.0, plot2)))
        val cond1 = plot3.interpolation(4) ~= 1.0

        if(!cond1) ko(s"cos^2 + sin^2 = $plot3")
        else ok
      }

      "normal(-1, 1) + normal(1, 1)" in {
        val n1 = NormalDist(-1.0, 1.0).cdfSampling
        val n2 = NormalDist(1.0, 1.0).cdfSampling
        val sum = (0.5, n1) :+ (0.5, n2)

        val cond1 = sum.records.head._2 ~= 0.0
        val cond2 = sum.records.last._2 ~= 1.0

        if(!cond1) ko(s"first point: ${sum.records.head}")
        else if (!cond2) ko(s"last point: ${sum.records.last}")
        else ok
      }

    }

    "inverse" in todo

    "normalizedInverse" in todo

    "inverseNormalizedCumulative" in {
      val domains = (1 to 100).map(i => -5 + i * 0.1).toArray
      val records = domains.map(i => (i, NormalDist.std.pdf(i)))
      val plot1 = PointPlot(records)
      val plot2 = plot1.inverseNormalizedCumulative
      val cond1 = plot2.interpolation(0.5) ~= (0.0, 0.05)
      val cond2 = plot2.records.head._1 >= 0 && plot2.records.last._1 <= 1

      if(!cond1) ko(s"${plot2.interpolation(0.5)}")
      if(!cond2) ko(s"first domain: ${plot2.records.head._1}, last domain: ${plot2.records.last._1}")
      else ok
    }

    "integralAll" in {
      val plot = PointPlot.deltas((1.0, 1.0) :: (1.5, 1.0) :: Nil, 1E-10)
      val integral = PointPlot.integralAll(plot)
      val cond1 = integral ~= 2

      if(!cond1) ko(s"expected: 2, calculated: $integral")
      else ok
    }

    /** End */

  }

}