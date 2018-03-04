package flip.pdf

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.syntax._

import scala.language.higherKinds

/**
  * PlottedDist, or Plotted Distribution has a plot with specific quantization
  * points and their probability densities.
  * */
trait PlottedDist[A] extends SamplingDist[A] {

  def sampling: DensityPlot

}

trait PlottedDistPropOps[D[_] <: PlottedDist[_]] extends SamplingDistPropOps[D] {

  def sampling[A](dist: D[A]): DensityPlot = dist.sampling

  def filter[A](dist: PlottedDist[A], f: RangeP => Boolean): PlottedDist[A] =
    PlottedDist.bare(
      dist.measure,
      DensityPlot.disjoint(dist.sampling.records.filter { case (range, _) => f(range) }),
      dist.conf
    )

  def probability[A](dist: D[A], start: A, end: A): Double = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    dist.sampling.integral(measure.to(start), measure.to(end))
  }

}

object PlottedDist extends PlottedDistPropOps[PlottedDist] {

  case class PlottedDistImpl[A](measure: Measure[A], sampling: DensityPlot, conf: SamplingDistConf)
      extends PlottedDist[A]

  def bare[A](measure: Measure[A], densityPlot: DensityPlot, conf: SamplingDistConf): PlottedDist[A] =
    PlottedDistImpl(measure, densityPlot, conf)

  def sample[A](dist: PlottedDist[A]): (PlottedDist[A], A) = ???

}
