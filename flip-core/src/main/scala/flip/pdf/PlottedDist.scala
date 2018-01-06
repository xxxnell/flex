package flip.pdf

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.syntax._

import scala.language.higherKinds

trait PlottedDist[A] extends SamplingDist[A] {

  def sampling: DensityPlot

}

trait PlottedDistPropOps[D[_]<:PlottedDist[_]] extends SamplingDistPropOps[D, SamplingDistConf] {

  def sampling[A](dist: D[A]): Option[DensityPlot] = Some(dist.sampling)

  def filter[A](dist: PlottedDist[A], f: RangeP => Boolean): PlottedDist[A] =
    PlottedDist(dist.measure, DensityPlot.disjoint(dist.sampling.records.filter { case (range, _) => f(range) }))

}

object PlottedDist extends PlottedDistPropOps[PlottedDist] {

  case class PlottedDistImpl[A](measure: Measure[A], sampling: DensityPlot) extends PlottedDist[A]

  def apply[A](measure: Measure[A], densityPlot: DensityPlot): PlottedDist[A] = PlottedDistImpl(measure, densityPlot)

  def probability[A](dist: PlottedDist[A], start: A, end: A): Option[Double] = ???

  def sample[A](dist: PlottedDist[A]): (PlottedDist[A], A) = ???

}
