package flip.pdf

import cats.implicits._
import flip.conf.{DistConf, SamplingDistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.arithmetic.CombinationDist
import flip.plot.{DensityPlot, PointPlot}
import flip.range.{RangeM, RangeP}
import flip.rand.IRng

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

  def sampling[A](dist: D[A]): PointPlot

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
    (0 until n).foldLeft[(D[A], List[A])]((dist, Nil)) {
      case ((utdDist1, acc), _) =>
        val (utdDist2, s) = sample(utdDist1)
        (utdDist2, s :: acc)
    }
  }

  def cdfPlot[A](dist: D[A]): PointPlot = {
    sampling(dist).normalizedCumulative
  }

  def icdfPlot[A](dist: D[A]): PointPlot = {
    sampling(dist).inverseNormalizedCumulative
  }

  def interpolationPdf[A](dist: D[A], a: A): Double = {
    val plot = sampling(dist)
    val p = dist.measure.asInstanceOf[Measure[A]].to(a)
    plot.interpolation(p)
  }

  def pdf[A](dist: D[A], a: A): Double = interpolationPdf(dist, a: A)

  def interpolationCdf[A](dist: D[A], a: A): Double = {
    val cdf = cdfPlot(dist)
    val p = dist.measure.asInstanceOf[Measure[A]].to(a)
    cdf.interpolation(p)
  }

  def cdf[A](dist: D[A], a: A): Double = interpolationCdf(dist, a)

  def interpolationIcdf[A](dist: D[A], p: Double): A = {
    val icdf = icdfPlot(dist)
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
    case smooth: SmoothDist[A] => SmoothDist.probability(smooth, start, end)
    case sampling: SamplingDist[A] => SamplingDist.probability(sampling, start, end)
    case combination: CombinationDist[A] => CombinationDist.probability(combination, start, end)
  }

  override def pdf[A](dist: Dist[A], a: A): Double = dist match {
    case smooth: SmoothDist[A] => SmoothDist.pdf(smooth, a)
    case sampling: SamplingDist[A] => SamplingDist.pdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.pdf(combination, a)
    case _ => super.pdf(dist, a)
  }

  override def cdf[A](dist: Dist[A], a: A): Double = dist match {
    case smooth: SmoothDist[A] => SmoothDist.cdf(smooth, a)
    case sampling: SamplingDist[A] => SamplingDist.cdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.cdf(combination, a)
    case _ => super.cdf(dist, a)
  }

  override def icdf[A](dist: Dist[A], p: Prim): A = dist match {
    case smooth: SmoothDist[A] => SmoothDist.icdf(smooth, p)
    case sampling: SamplingDist[A] => SamplingDist.icdf(sampling, p)
    case combination: CombinationDist[A] => CombinationDist.icdf(combination, p)
    case _ => super.icdf(dist, p)
  }

  def modifyRng[A](dist: Dist[A], f: IRng => IRng): Dist[A] = dist match {
    case smooth: SmoothDist[A] => SmoothDist.modifyRng(smooth, f)
    case sampling: SamplingDist[A] => SamplingDist.modifyRng(sampling, f)
    case combination: CombinationDist[A] => CombinationDist.modifyRng(combination, f)
  }

  def sampling[A](dist: Dist[A]): PointPlot = dist match {
    case smooth: SmoothDist[A] => SmoothDist.sampling(smooth)
    case sampling: SamplingDist[A] => SamplingDist.sampling(sampling)
    case combination: CombinationDist[A] => CombinationDist.sampling(combination)
  }

  override def sample[A](dist: Dist[A]): (Dist[A], A) = dist match {
    case combi: CombinationDist[A] => CombinationDist.sample(combi)
    case _ => super.sample(dist)
  }

}
