package flip.plot

import org.specs2.mutable._
import org.specs2.ScalaCheck
import cats.data.NonEmptyList
import flip.measure.syntax._
import flip.pdf.NormalDist

class PointPlotSpec extends Specification with ScalaCheck {

  "PointPlot" should {

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

      println(plot)

      if(!cond1) ko(s"expected: 2, calculated: $integral")
      else ok
    }

  }

}