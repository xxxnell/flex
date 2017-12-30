package sketch.scope.pdf

import sketch.scope.conf.SamplingDistConf
import sketch.scope.measure.Measure
import sketch.scope.plot.DensityPlot
import sketch.scope.range.RangeM

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SamplingDist[A] extends Dist[A]

trait SamplingDistPropOps[D[_]<:SamplingDist[_], C<:SamplingDistConf]
  extends DistPropOps[D, C]
    with SamplingDistPropLaws[D, C] {

  def densityPlot(dist: D[_]): Option[DensityPlot]

}

trait SamplingDistPropLaws[D[_]<:SamplingDist[_], C<:SamplingDistConf] { self: SamplingDistPropOps[D, C] =>

  def pdf[A](dist: D[A], a: A): Option[Double] = for {
    plot <- densityPlot(dist)
    density = DensityPlot.interpolation(plot, dist.measure.asInstanceOf[Measure[A]].to(a))
  } yield density

}

object SamplingDist extends SamplingDistPropOps[SamplingDist, SamplingDistConf] {

  def forSmoothDist[A](dist: SmoothDist[A], domains: List[RangeM[A]]): Option[SamplingDist[A]] =
    dist.toSamplingDist(domains)

  def probability[A](dist: SamplingDist[A], start: A, end: A): Option[Double] = dist match {
    case sketch: Sketch[A] => Sketch.probability(sketch, start, end)
    case plotted: PlottedDist[A] => PlottedDist.probability(plotted, start, end)
    case _ => ???
  }

  def densityPlot(dist: SamplingDist[_]): Option[DensityPlot] = dist match {
    case sketch: Sketch[_] => Sketch.densityPlot(sketch)
    case plotted: PlottedDist[_] => PlottedDist.densityPlot(plotted)
    case _ => ???
  }

  def sample[A](dist: SamplingDist[A]): (SamplingDist[A], A) = dist match {
    case sketch: Sketch[_] => Sketch.sample(sketch)
    case plotted: PlottedDist[_] => PlottedDist.sample(plotted)
  }

}