package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.pdf.sampling.IcdfSampling
import flip.plot.{DensityPlot, PointPlot}
import flip.rand.IRng

import scala.language.higherKinds

/**
  * SmoothDist, or Smooth distribution is the opposite of SamplingDist.
  * */
trait SmoothDist[A] extends Dist[A] {

  def conf: SmoothDistConf

}

trait SmoothDistPropOps[D[_] <: SmoothDist[_]] extends DistPropOps[D] with SmoothDistPropLaws[D] {}

trait SmoothDistPropLaws[D[_] <: SmoothDist[_]] { self: SmoothDistPropOps[D] =>

  def sampling[A](dist: D[A]): PointPlot = {
    val icdf: Double => A = p => self.icdf(dist, p)
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val records = IcdfSampling.sampling(icdf, measure, dist.conf.sampling).map(p => (measure.to(p), pdf(dist, p)))
    PointPlot.unsafe(records.toArray)
  }

}

object SmoothDist extends SmoothDistPropOps[SmoothDist] {

  def modifyRng[A](dist: SmoothDist[A], f: IRng => IRng): SmoothDist[A] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.modifyRng(predefined, f)
    case numeric: NumericDist[A] => NumericDist.modifyRng(numeric, f)
  }

  def probability[A](dist: SmoothDist[A], start: A, end: A): Prim = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.probability(predefined, start, end)
    case numeric: NumericDist[A] => NumericDist.probability(numeric, start, end)
  }

  // overrides

  override def pdf[A](dist: SmoothDist[A], a: A): Double = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.pdf(predefined, a)
    case numeric: NumericDist[A] => NumericDist.pdf(numeric, a)
  }

  override def cdf[A](dist: SmoothDist[A], a: A): Double = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.cdf(predefined, a)
    case numeric: NumericDist[A] => NumericDist.cdf(numeric, a)
  }

  override def icdf[A](dist: SmoothDist[A], p: Double): A = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.icdf(predefined, p)
    case numeric: NumericDist[A] => NumericDist.icdf(numeric, p)
  }

  override def sampling[A](dist: SmoothDist[A]): PointPlot = dist match {
    case dist: NumericDist[A] => NumericDist.sampling(dist)
    case _ => super.sampling(dist)
  }

}
