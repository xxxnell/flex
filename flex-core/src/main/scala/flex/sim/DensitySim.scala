package flex.sim

import flex.pdf.Dist
import flex.plot.PointPlot

trait DensitySim {

  def point(value1: Double, value2: Double): Double

  def sim[A](sampling1: PointPlot, sampling2: PointPlot): Double = {
    simDensity(sampling1, sampling2).integralAll
  }

  def simDensity[A](pdfSampling1: PointPlot, pdfSampling2: PointPlot): PointPlot = {
    val density1 = pdfSampling1.map { case (x, y) => (x, point(y, pdfSampling2.of(x))) }
    val density2 = pdfSampling2.map { case (x, y) => (x, point(pdfSampling1.of(x), y)) }

    PointPlot.safe(density1.records ++ density2.records)
  }

  def simForDist[A](d1: Dist[A], d2: Dist[A]): Double = {
    sim(d1.pdfSampling, d2.pdfSampling)
  }

  def simDensityForDist[A](d1: Dist[A], d2: Dist[A]): PointPlot =
    simDensity(d1.pdfSampling, d2.pdfSampling)

}
