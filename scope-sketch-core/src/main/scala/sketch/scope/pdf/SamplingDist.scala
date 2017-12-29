package sketch.scope.pdf

import sketch.scope.measure.Measure
import sketch.scope.plot.{DensityPlot, Plot}
import sketch.scope.range.{RangeM, RangeP}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SamplingDist[A] extends Dist[A]

trait SamplingDistPropOps[D[_]<:SamplingDist[_]] extends DistPropOps[D] with SamplingDistPropLaws[D] {

  def densityPlot(dist: D[_]): Option[DensityPlot]

}

trait SamplingDistPropLaws[D[_]<:SamplingDist[_]] { self: SamplingDistPropOps[D] =>

  def pdf[A](dist: D[A], a: A): Option[Double] = for {
    plot <- densityPlot(dist)
    density = plot.interpolation(dist.measure.asInstanceOf[Measure[A]].to(a))
  } yield density

}

object SamplingDist extends SamplingDistPropOps[SamplingDist] {

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