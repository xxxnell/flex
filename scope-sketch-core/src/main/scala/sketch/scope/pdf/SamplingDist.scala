package sketch.scope.pdf

import sketch.scope.conf.{SamplingDistConf, SketchConf}
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

  def sampling[A](dist: D[A], conf: C): Option[DensityPlot]

}

trait SamplingDistPropLaws[D[_]<:SamplingDist[_], C<:SamplingDistConf] { self: SamplingDistPropOps[D, C] =>

  def interpolationPdf[A](dist: D[A], a: A, conf: C): Option[Double] = for {
    plot <- sampling(dist, conf)
    density = DensityPlot.interpolation(plot, dist.measure.asInstanceOf[Measure[A]].to(a))
  } yield density

}

object SamplingDist extends SamplingDistPropOps[SamplingDist, SamplingDistConf] {

  def forSmoothDist[A](dist: SmoothDist[A], domains: List[RangeM[A]]): Option[SamplingDist[A]] =
    dist.sampling(domains)

  def probability[A](dist: SamplingDist[A],
                     start: A,
                     end: A,
                     conf: SamplingDistConf): Option[Double] = (dist, conf) match {
    case (sketch: Sketch[A], conf: SketchConf) => Sketch.probability(sketch, start, end, conf)
    case (plotted: PlottedDist[A], _) => PlottedDist.probability(plotted, start, end, conf)
    case _ => ???
  }

  def sampling[A](dist: SamplingDist[A], conf: SamplingDistConf): Option[DensityPlot] = (dist, conf) match {
    case (sketch: Sketch[A], conf: SketchConf) => Sketch.sampling(sketch, conf)
    case (plotted: PlottedDist[A], _) => PlottedDist.sampling(plotted, conf)
    case _ => ???
  }

  def sample[A](dist: SamplingDist[A]): (SamplingDist[A], A) = dist match {
    case sketch: Sketch[_] => Sketch.sample(sketch)
    case plotted: PlottedDist[_] => PlottedDist.sample(plotted)
  }

  override def pdf[A](dist: SamplingDist[A], a: A, conf: SamplingDistConf): Option[Prim] = (dist, conf) match {
    case (sketch: Sketch[_], conf: SketchConf) => Sketch.fastPdf(sketch, a, conf)
    case _ => super.interpolationPdf(dist, a, conf)
  }

}