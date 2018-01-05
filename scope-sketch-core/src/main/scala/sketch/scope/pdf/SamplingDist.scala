package sketch.scope.pdf

import sketch.scope.conf.SamplingDistConf
import sketch.scope.measure.Measure
import sketch.scope.plot.DensityPlot
import sketch.scope.range.RangeM

import scala.language.higherKinds

/**
  * Sampling distribution provides the sampling points and its probability density value.
  * */
trait SamplingDist[A] extends Dist[A]

trait SamplingDistPropOps[D[_]<:SamplingDist[_], C<:SamplingDistConf]
  extends DistPropOps[D, C]
    with SamplingDistPropLaws[D, C] {

  def sampling[A](dist: D[A]): Option[DensityPlot]

}

trait SamplingDistPropLaws[D[_]<:SamplingDist[_], C<:SamplingDistConf] { self: SamplingDistPropOps[D, C] =>

  def interpolationPdf[A](dist: D[A], a: A): Option[Double] = for {
    plot <- sampling(dist)
    density = DensityPlot.interpolation(plot, dist.measure.asInstanceOf[Measure[A]].to(a))
  } yield density

}

object SamplingDist extends SamplingDistPropOps[SamplingDist, SamplingDistConf] {

  def forSmoothDist[A](dist: SmoothDist[A], domains: List[RangeM[A]]): Option[SamplingDist[A]] =
    dist.sampling(domains)

  def probability[A](dist: SamplingDist[A], start: A, end: A): Option[Double] = dist match {
    case sketch: Sketch[A] => Sketch.probability(sketch, start, end)
    case plotted: PlottedDist[A] => PlottedDist.probability(plotted, start, end)
    case _ => ???
  }

  def sampling[A](dist: SamplingDist[A]): Option[DensityPlot] = dist match {
    case sketch: Sketch[A] => Sketch.sampling(sketch)
    case plotted: PlottedDist[A] => PlottedDist.sampling(plotted)
    case _ => ???
  }

  def sample[A](dist: SamplingDist[A]): (SamplingDist[A], A) = dist match {
    case sketch: Sketch[_] => Sketch.sample(sketch)
    case plotted: PlottedDist[_] => PlottedDist.sample(plotted)
  }

  def pdf[A](dist: SamplingDist[A], a: A): Option[Prim] = dist match {
    case sketch: Sketch[_] => Sketch.fastPdf(sketch, a)
    case _ => super.interpolationPdf(dist, a)
  }

}