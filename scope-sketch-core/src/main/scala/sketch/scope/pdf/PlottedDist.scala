package sketch.scope.pdf

import sketch.scope.conf.SamplingDistConf
import sketch.scope.measure.Measure
import sketch.scope.plot.DensityPlot
import sketch.scope.range.syntax._

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait PlottedDist[A] extends SamplingDist[A] {

  def densityPlot: DensityPlot

}

trait PlottedDistPropOps[D[_]<:PlottedDist[_]] extends SamplingDistPropOps[D, SamplingDistConf] {

  def sampling(dist: D[_]): Option[DensityPlot] = Some(dist.densityPlot)

  def filter[A](dist: PlottedDist[A], f: RangeP => Boolean): PlottedDist[A] =
    PlottedDist(dist.measure, DensityPlot.disjoint(dist.densityPlot.records.filter { case (range, _) => f(range) }))

}

object PlottedDist extends PlottedDistPropOps[PlottedDist] {

  case class PlottedDistImpl[A](measure: Measure[A], densityPlot: DensityPlot) extends PlottedDist[A]

  def apply[A](measure: Measure[A], densityPlot: DensityPlot): PlottedDist[A] = PlottedDistImpl(measure, densityPlot)

  def probability[A](dist: PlottedDist[A], start: A, end: A): Option[Double] = ???

  def sample[A](dist: PlottedDist[A]): (PlottedDist[A], A) = ???

}
