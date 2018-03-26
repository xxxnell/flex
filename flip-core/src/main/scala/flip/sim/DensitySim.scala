package flip.sim

import flip.pdf.{Dist, Prim, SamplingDist}
import flip.plot.DensityPlot

trait DensitySim {

  def point(value1: Double, value2: Double): Double

  def sim[A](sampling1: DensityPlot, pdf1: Prim => Double, sampling2: DensityPlot, pdf2: Prim => Double): Double = {
    simDensity(sampling1, pdf1, sampling2, pdf2).integralAll
  }

  def simDensity[A](sampling1: DensityPlot,
                    pdf1: Prim => Double,
                    sampling2: DensityPlot,
                    pdf2: Prim => Double): DensityPlot = {
    val density1 = sampling1.modify { case (range, value) => point(value, pdf2(range.cutoffMiddle)) }
    val density2 = sampling2.modify { case (range, value) => point(pdf1(range.cutoffMiddle), value) }

    density1 ++ density2
  }

  def simForDist[A](d1: Dist[A], d2: Dist[A]): Double = {
    sim(d1.sampling, p => d1.pdf(d1.measure.from(p)), d2.sampling, p => d2.pdf(d2.measure.from(p)))
  }

  def simDensityForDist[A](d1: Dist[A], d2: Dist[A]): DensityPlot =
    simDensity(d1.sampling, p => d1.pdf(d1.measure.from(p)), d2.sampling, p => d2.pdf(d2.measure.from(p)))

}
