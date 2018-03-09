package flip.pdf

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.syntax._
import flip.rand.IRng

import scala.language.higherKinds

/**
  * PlottedDist, or Plotted Distribution has a plot with specific quantization
  * points and their probability densities.
  * */
trait PlottedDist[A] extends SamplingDist[A] {

  def sampling: DensityPlot

}

trait PlottedDistPropOps[D[_] <: PlottedDist[_]] extends SamplingDistPropOps[D] {

  def modifySampling[A](dist: D[A], f: DensityPlot => DensityPlot): D[A]

  def sampling[A](dist: D[A]): DensityPlot = dist.sampling

  def filter[A](dist: D[A], f: RangeP => Boolean): D[A] =
    modifySampling(dist, sampling => DensityPlot.disjoint(sampling.records.filter { case (range, _) => f(range) }))

  def probability[A](dist: D[A], start: A, end: A): Double = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    dist.sampling.integral(measure.to(start), measure.to(end))
  }

}

object PlottedDist extends PlottedDistPropOps[PlottedDist] {

  private case class PlottedDistImpl[A](measure: Measure[A], rng: IRng, sampling: DensityPlot, conf: SamplingDistConf)
      extends PlottedDist[A]

  def bare[A](measure: Measure[A], rng: IRng, densityPlot: DensityPlot, conf: SamplingDistConf): PlottedDist[A] =
    PlottedDistImpl(measure, rng, densityPlot, conf)

  def sample[A](dist: PlottedDist[A]): (PlottedDist[A], A) = ???

  def modifyRng[A](dist: PlottedDist[A], f: IRng => IRng): PlottedDist[A] =
    bare(dist.measure, f(dist.rng), dist.sampling, dist.conf)

  def modifySampling[A](dist: PlottedDist[A], f: DensityPlot => DensityPlot): PlottedDist[A] =
    bare(dist.measure, dist.rng, f(dist.sampling), dist.conf)

}
