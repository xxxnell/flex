package flex.pdf

import flex.conf.pdf.SmoothDistConf
import flex.measure.Measure
import flex.pdf.sampling.IcdfSampling
import flex.plot.PointPlot
import flex.rand.IRng

import scala.language.higherKinds

/**
 * SmoothDist, or Smooth distribution is the opposite of SamplingDist.
 * */
trait SmoothDist[A] extends Dist[A] {

  def conf: SmoothDistConf

}

trait SmoothDistPropOps[D[_] <: SmoothDist[_]] extends DistPropOps[D] with SmoothDistPropLaws[D] {}

trait SmoothDistPropLaws[D[_] <: SmoothDist[_]] { self: SmoothDistPropOps[D] =>

  override def pdfSampling[A](dist: D[A]): PointPlot = {
    val icdf: Double => A = p => self.icdf(dist, p)
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val domain = IcdfSampling.sampling(icdf, measure, dist.conf.sampling).map(a => measure.to(a)).toArray
    var i = 0
    val records = Array.ofDim[(Double, Double)](domain.length)
    while (i < domain.length) {
      val p = domain.apply(i)
      val a = measure.from(p)
      if (i == 0 || i == domain.length - 1) records.update(i, (p, 0.0))
      else records.update(i, (p, pdf(dist, a)))
      i += 1
    }

    PointPlot.unsafe(records)
  }

  override def cdfSampling[A](dist: D[A]): PointPlot = {
    val icdf: Double => A = p => self.icdf(dist, p)
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val domain = IcdfSampling.sampling(icdf, measure, dist.conf.sampling).map(a => measure.to(a)).toArray
    var i = 0
    val records = Array.ofDim[(Double, Double)](domain.length)
    while (i < domain.length) {
      val p = domain.apply(i)
      val a = measure.from(p)
      if (i == 0) records.update(i, (p, 0.0))
      else if (i == domain.length - 1) records.update(i, (p, 1.0))
      else records.update(i, (p, cdf(dist, a)))
      i += 1
    }

    PointPlot.unsafe(records)
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

  override def pdfSampling[A](dist: SmoothDist[A]): PointPlot = dist match {
    case dist: NumericDist[A] => NumericDist.pdfSampling(dist)
    case _ => super.pdfSampling(dist)
  }

  override def cdfSampling[A](dist: SmoothDist[A]): PointPlot = dist match {
    case dist: NumericDist[A] => NumericDist.cdfSampling(dist)
    case _ => super.cdfSampling(dist)
  }

}
