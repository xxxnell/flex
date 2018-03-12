package flip.pdf

import cats.implicits._
import flip.conf.{DistConf, SamplingDistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.arithmetic.CombinationDist
import flip.plot.DensityPlot
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

  def sample[A](dist: D[A]): (D[A], A)

  def modifyRng[A](dist: D[A], f: IRng => IRng): D[A]

}

trait DistPropLaws[D[_] <: Dist[_]] { self: DistPropOps[D] =>

  def numericPdf[A](dist: D[A], a: A): Double = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val delta = dist.conf.delta
    val ap = measure.from(measure.to(a) + delta)

    probability(dist, a, ap) / delta
  }

  def pdf[A](dist: D[A], a: A): Double

  def cdf[A](dist: D[A], a: A): Double

  def icdf[A](dist: D[A], p: Double): A

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

  def sampling[A](probability: (A, A) => Double, domains: List[RangeM[A]]): DensityPlot = {
    val records = domains
      .filter(range => !range.isPoint)
      .map(range => (RangeP.forRangeM(range), (probability(range.start, range.end) / range.length).toDouble))

    DensityPlot.disjoint(records)
  }

  def samplingForDomain[A](dist: D[A], domains: List[RangeM[A]]): DensityPlot = {
    sampling((start: A, end: A) => probability(dist, start, end), domains)
  }

  def samplingDist[A](dist: D[A], domains: List[RangeM[A]]): PlottedDist[A] = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val plot = samplingForDomain(dist, domains)
    val conf = SamplingDistConf.forDistConf(dist.conf)

    PlottedDist.bare(measure, dist.rng, plot, conf)
  }

  def samplingDistForPlottedDist[A](dist: D[A], pltDist: PlottedDist[A]): PlottedDist[A] = {
    val densityPlot = pltDist.sampling
    val domainsP = densityPlot.records.map(_._1)
    val domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(pltDist.measure))

    samplingDist(dist, domainsM)
  }

  def samplingDistForSamplingDist[A](dist: D[A], smplDist: SamplingDist[A]): PlottedDist[A] = {
    val densityPlot = smplDist.sampling
    val domainsP = densityPlot.records.map(_._1)
    val domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(smplDist.measure))

    samplingDist(dist, domainsM)
  }

  def uniformSampling[A](dist: D[A], start: A, end: A, size: Int): PlottedDist[A] = {
    val domains = RangeM(start, end)(dist.measure.asInstanceOf[Measure[A]]).uniformSplit(size)

    samplingDist(dist, domains)
  }

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

  def pdf[A](dist: Dist[A], a: A): Double = dist match {
    case smooth: SmoothDist[A] => SmoothDist.pdf(smooth, a)
    case sampling: SamplingDist[A] => SamplingDist.pdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.pdf(combination, a)
  }

  def cdf[A](dist: Dist[A], a: A): Double = dist match {
    case smooth: SmoothDist[A] => SmoothDist.cdf(smooth, a)
    case sampling: SamplingDist[A] => SamplingDist.cdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.cdf(combination, a)
  }

  override def icdf[A](dist: Dist[A], p: Prim): A = dist match {
    case smooth: SmoothDist[A] => SmoothDist.icdf(smooth, p)
    case sampling: SamplingDist[A] => SamplingDist.icdf(sampling, p)
    case combination: CombinationDist[A] => CombinationDist.icdf(combination, p)
  }

  def modifyRng[A](dist: Dist[A], f: IRng => IRng): Dist[A] = dist match {
    case smooth: SmoothDist[A] => SmoothDist.modifyRng(smooth, f)
    case sampling: SamplingDist[A] => SamplingDist.modifyRng(sampling, f)
    case combination: CombinationDist[A] => CombinationDist.modifyRng(combination, f)
  }

  override def sample[A](dist: Dist[A]): (Dist[A], A) = dist match {
    case combi: CombinationDist[A] => CombinationDist.sample(combi)
    case _ => super.sample(dist)
  }

}
