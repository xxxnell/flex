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

  def sampling[A](dist: D[A], conf: SamplingDistConf): Option[DensityPlot] = Some(dist.sampling)

  def filter[A](dist: PlottedDist[A], f: RangeP => Boolean): PlottedDist[A] =
    PlottedDist(dist.measure, DensityPlot.disjoint(dist.sampling.records.filter { case (range, _) => f(range) }))

  def probability[A](dist: D[A], start: A, end: A, conf: SamplingDistConf): Option[Double] = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    Some(dist.sampling.integral(measure.to(start), measure.to(end)))
  }

}

object PlottedDist extends PlottedDistPropOps[PlottedDist] {

  case class PlottedDistImpl[A](measure: Measure[A], sampling: DensityPlot) extends PlottedDist[A]

  def apply[A](measure: Measure[A], densityPlot: DensityPlot): PlottedDist[A] = PlottedDistImpl(measure, densityPlot)

  def sample[A](dist: PlottedDist[A]): (PlottedDist[A], A) = ???

}
