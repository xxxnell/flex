package flip.sim

import flip.pdf.Dist
import flip.plot.PointPlot

trait DensitySim {

  def point(value1: Double, value2: Double): Double

  def sim[A](sampling1: PointPlot, sampling2: PointPlot): Double = {
//    simDensity(sampling1, pdf1, sampling2, pdf2).integralAll
    ???
  }

  def simDensity[A](sampling1: PointPlot,
                    sampling2: PointPlot): PointPlot = {
//    val density1 = sampling1.modify { case (range, value) => point(value, pdf2(range.cutoffMiddle)) }
//    val density2 = sampling2.modify { case (range, value) => point(pdf1(range.cutoffMiddle), value) }
//
//    density1 ++ density2
    ???
  }

  def simForDist[A](d1: Dist[A], d2: Dist[A]): Double = {
    sim(d1.sampling, d2.sampling)
  }

  def simDensityForDist[A](d1: Dist[A], d2: Dist[A]): PointPlot =
    simDensity(d1.sampling, d2.sampling)

}
