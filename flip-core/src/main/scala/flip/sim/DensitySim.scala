package flip.sim

import flip.pdf.Dist
import flip.plot.PointPlot

trait DensitySim {

  def point(value1: Double, value2: Double): Double

  def sim[A](sampling1: PointPlot, sampling2: PointPlot): Double = {
    simDensity(sampling1, sampling2).integralAll
  }

  def simDensity[A](sampling1: PointPlot, sampling2: PointPlot): PointPlot = {
    val density1 = sampling1.map { case (x, y) => (x, point(y, sampling2.of(x))) }
    val density2 = sampling2.map { case (x, y) => (x, point(sampling1.of(x), y)) }

    PointPlot.unsafe((density1.records ++ density2.records).sortBy(_._1))
  }

  def simForDist[A](d1: Dist[A], d2: Dist[A]): Double = {
    sim(d1.sampling, d2.sampling)
  }

  def simDensityForDist[A](d1: Dist[A], d2: Dist[A]): PointPlot =
    simDensity(d1.sampling, d2.sampling)

}
