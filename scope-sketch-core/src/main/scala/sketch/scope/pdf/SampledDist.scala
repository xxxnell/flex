package sketch.scope.pdf

import sketch.scope.measure.Measure
import sketch.scope.plot.{DensityPlot, Plot}
import sketch.scope.range.{RangeM, RangeP}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampledDist[A] extends Dist[A]

trait SampleDistPropOps[D[_]<:SampledDist[_]] extends DistPropOps[D] with SampleDistPropLaws[D] {

  def densityPlot(dist: D[_]): Option[DensityPlot]

}

trait SampleDistPropLaws[D[_]<:SampledDist[_]] { self: SampleDistPropOps[D] =>

  def pdf[A](dist: D[A], a: A): Option[Double] = for {
    plot <- densityPlot(dist)
    density = plot.interpolation(dist.measure.asInstanceOf[Measure[A]].to(a))
  } yield density

}

object SampledDist extends SampleDistPropOps[SampledDist] {

  def forSmoothDist[A](dist: SmoothDist[A], domains: List[RangeM[A]]): Option[SampledDist[A]] =
    dist.toSampleDist(domains)

  def probability[A](dist: SampledDist[A], start: A, end: A): Option[Double] = dist match {
    case sketch: Sketch[A] => Sketch.probability(sketch, start, end)
    case plotted: PlottedDist[A] => PlottedDist.probability(plotted, start, end)
    case _ => ???
  }

  def densityPlot(dist: SampledDist[_]): Option[DensityPlot] = dist match {
    case sketch: Sketch[_] => Sketch.densityPlot(sketch)
    case plotted: PlottedDist[_] => PlottedDist.densityPlot(plotted)
    case _ => ???
  }

  def sample[A](dist: SampledDist[A]): (SampledDist[A], A) = dist match {
    case sketch: Sketch[_] => Sketch.sample(sketch)
    case plotted: PlottedDist[_] => PlottedDist.sample(plotted)
  }

}