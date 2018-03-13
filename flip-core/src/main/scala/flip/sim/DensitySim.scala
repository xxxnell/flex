package flip.sim

import flip.pdf.{Dist, Prim, SamplingDist}
import flip.plot.DensityPlot

trait DensitySim {

  def point(value1: Double, value2: Double): Double

  def sim(sampling: DensityPlot, pdf: Prim => Double): Double =
    simDensity(sampling, pdf).integralAll

  def simDensity[A](sampling: DensityPlot, pdf: Prim => Double): DensityPlot =
    sampling.modify { case (range, value) => point(value, pdf(range.middle)) }

  def simForDist[A](d1: Dist[A], d2: Dist[A]): Double =
    sim(d1.sampling, p => d2.pdf(d2.measure.from(p)))

  def simDensityForDist[A](d1: Dist[A], d2: Dist[A]): DensityPlot =
    simDensity(d1.sampling, p => d2.pdf(d2.measure.from(p)))

}
