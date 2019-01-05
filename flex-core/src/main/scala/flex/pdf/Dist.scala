package flex.pdf

import flex.conf.pdf.{DistConf, SmoothDistConf}
import flex.measure.Measure
import flex.pdf.arithmetic.CombinationDist
import flex.plot.PointPlot
import flex.rand.IRng

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

/**
 * Distribution, or probability distribution provides to get probability for
 * given domain.
 * */
trait Dist[A] {

  def measure: Measure[A]

  def rng: IRng

  def conf: DistConf

}

trait DistPropOps[D[_] <: Dist[_]] extends DistPropLaws[D] {

  def probability[A](dist: D[A], start: A, end: A): Double

  def modifyRng[A](dist: D[A], f: IRng => IRng): D[A]

  def cdfSampling[A](dist: D[A]): PointPlot

}

trait DistPropLaws[D[_] <: Dist[_]] { self: DistPropOps[D] =>

  def numericPdf[A](dist: D[A], a: A): Double = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val delta = dist.conf.delta
    val ap = measure.from(measure.to(a) + delta)

    probability(dist, a, ap) / delta
  }

  def sample[A](dist: D[A]): (D[A], A) = {
    val (rng, rand) = dist.rng.next
    (modifyRng(dist, _ => rng), icdf(dist, rand))
  }

  def samples[A](dist: D[A], n: Int): (D[A], List[A]) = {
    var (i, _dist) = (0, dist)
    val acc = new ListBuffer[A]
    while (i < n) {
      val (_dist1, s) = sample(_dist)
      acc.append(s)
      _dist = _dist1
      i += 1
    }
    (_dist, acc.toList)
  }

  def pdfSampling[A](dist: D[A]): PointPlot =
    cdfSampling(dist).changeRate

  def icdfSampling[A](dist: D[A]): PointPlot =
    cdfSampling(dist).inverse

  def interpolationPdf[A](dist: D[A], a: A): Double = {
    val plot = pdfSampling(dist)
    val p = dist.measure.asInstanceOf[Measure[A]].to(a)
    plot.interpolation(p)
  }

  def pdf[A](dist: D[A], a: A): Double = interpolationPdf(dist, a: A)

  def interpolationCdf[A](dist: D[A], a: A): Double = {
    val cdf = cdfSampling(dist)
    val p = dist.measure.asInstanceOf[Measure[A]].to(a)
    cdf.interpolation(p)
  }

  def cdf[A](dist: D[A], a: A): Double = interpolationCdf(dist, a)

  def interpolationIcdf[A](dist: D[A], p: Double): A = {
    val icdf = icdfSampling(dist)
    val measure = dist.measure.asInstanceOf[Measure[A]]

    measure.from(icdf.interpolation(p))
  }

  def icdf[A](dist: D[A], p: Double): A = interpolationIcdf(dist, p)

}

object Dist extends DistPropOps[Dist] { self =>

  def delta[A](center: A)(implicit measure: Measure[A], conf: SmoothDistConf): Dist[A] =
    DeltaDist(measure, conf, center)

  def normal[A](mean: A, variance: Double)(implicit measure: Measure[A], conf: SmoothDistConf): NormalDist[A] =
    NormalDist(mean, variance)

  // pipelining

  def probability[A](dist: Dist[A], start: A, end: A): Double = dist match {
    case smooth: SmoothDist[A]           => SmoothDist.probability(smooth, start, end)
    case sampling: SamplingDist[A]       => SamplingDist.probability(sampling, start, end)
    case combination: CombinationDist[A] => CombinationDist.probability(combination, start, end)
  }

  override def pdf[A](dist: Dist[A], a: A): Double = dist match {
    case smooth: SmoothDist[A]           => SmoothDist.pdf(smooth, a)
    case sampling: SamplingDist[A]       => SamplingDist.pdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.pdf(combination, a)
    case _                               => super.pdf(dist, a)
  }

  override def cdf[A](dist: Dist[A], a: A): Double = dist match {
    case smooth: SmoothDist[A]           => SmoothDist.cdf(smooth, a)
    case sampling: SamplingDist[A]       => SamplingDist.cdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.cdf(combination, a)
    case _                               => super.cdf(dist, a)
  }

  override def icdf[A](dist: Dist[A], p: Prim): A = dist match {
    case smooth: SmoothDist[A]           => SmoothDist.icdf(smooth, p)
    case sampling: SamplingDist[A]       => SamplingDist.icdf(sampling, p)
    case combination: CombinationDist[A] => CombinationDist.icdf(combination, p)
    case _                               => super.icdf(dist, p)
  }

  def modifyRng[A](dist: Dist[A], f: IRng => IRng): Dist[A] = dist match {
    case smooth: SmoothDist[A]           => SmoothDist.modifyRng(smooth, f)
    case sampling: SamplingDist[A]       => SamplingDist.modifyRng(sampling, f)
    case combination: CombinationDist[A] => CombinationDist.modifyRng(combination, f)
  }

  override def pdfSampling[A](dist: Dist[A]): PointPlot = dist match {
    case smooth: SmoothDist[A]           => SmoothDist.pdfSampling(smooth)
    case sampling: SamplingDist[A]       => SamplingDist.pdfSampling(sampling)
    case combination: CombinationDist[A] => CombinationDist.pdfSampling(combination)
  }

  def cdfSampling[A](dist: Dist[A]): PointPlot = dist match {
    case smooth: SmoothDist[A]           => SmoothDist.cdfSampling(smooth)
    case sampling: SamplingDist[A]       => SamplingDist.cdfSampling(sampling)
    case combination: CombinationDist[A] => CombinationDist.cdfSampling(combination)
  }

  override def sample[A](dist: Dist[A]): (Dist[A], A) = dist match {
    case combi: CombinationDist[A] => CombinationDist.sample(combi)
    case _                         => super.sample(dist)
  }

}
