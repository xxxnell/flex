package flip.pdf

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.RangeM

import scala.language.higherKinds

/**
  * SamplingDist, or Sampling distribution provides the sampling points and its
  * probability density.
  * */
trait SamplingDist[A] extends Dist[A] {

  def conf: SamplingDistConf

}

trait SamplingDistPropOps[D[_]<:SamplingDist[_]]
  extends DistPropOps[D]
    with SamplingDistPropLaws[D] {

  def sampling[A](dist: D[A]): Option[DensityPlot]

}

trait SamplingDistPropLaws[D[_]<:SamplingDist[_]] { self: SamplingDistPropOps[D] =>

  def interpolationPdf[A](dist: D[A], a: A): Option[Double] = for {
    plot <- sampling(dist)
    density = DensityPlot.interpolation(plot, dist.measure.asInstanceOf[Measure[A]].to(a))
  } yield density

}

object SamplingDist extends SamplingDistPropOps[SamplingDist] {

  def forSmoothDist[A](dist: SmoothDist[A], domains: List[RangeM[A]]): Option[SamplingDist[A]] =
    SmoothDist.samplingDist(dist, domains)

  def probability[A](dist: SamplingDist[A],
                     start: A,
                     end: A): Option[Double] = dist match {
    case (sketch: Sketch[A]) => Sketch.probability(sketch, start, end)
    case (plotted: PlottedDist[A]) => PlottedDist.probability(plotted, start, end)
    case _ => ???
  }

  def sampling[A](dist: SamplingDist[A]): Option[DensityPlot] = dist match {
    case (sketch: Sketch[A]) => Sketch.sampling(sketch)
    case (plotted: PlottedDist[A]) => PlottedDist.sampling(plotted)
    case _ => ???
  }

  def sample[A](dist: SamplingDist[A]): (SamplingDist[A], A) = dist match {
    case sketch: Sketch[_] => Sketch.sample(sketch)
    case plotted: PlottedDist[_] => PlottedDist.sample(plotted)
  }

  override def pdf[A](dist: SamplingDist[A], a: A): Option[Prim] = dist match {
    case (sketch: Sketch[_]) => Sketch.fastPdf(sketch, a)
    case _ => super.interpolationPdf(dist, a)
  }

}