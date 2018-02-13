package flip.pdf

import cats.implicits._
import flip.conf.{DistConf, SamplingDistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.arithmetic.CombinationDist
import flip.plot.DensityPlot
import flip.range.{RangeM, RangeP}

import scala.language.higherKinds

/**
  * Distribution, or probability distribution provides to get probability for
  * given domain.
  * */
trait Dist[A] {

  def measure: Measure[A]

  def conf: DistConf

}

trait DistPropOps[D[_]<:Dist[_]] extends DistPropLaws[D] {

  def probability[A](dist: D[A], start: A, end: A): Option[Double]

  def sample[A](dist: D[A]): (D[A], A)

}

trait DistPropLaws[D[_]<:Dist[_]] { self: DistPropOps[D] =>

  def numericPdf[A](dist: D[A], a: A): Option[Double] = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val delta = dist.conf.delta
    val ap = measure.from(measure.to(a) + delta)

    probability(dist, a, ap).map(prob => prob / delta)
  }

  def pdf[A](dist: D[A], a: A): Option[Double] = ???

  def cdf[A](sketch: D[A], a: A): Option[Double] = ???

  def samples[A](dist: D[A], n: Int): (D[A], List[A]) = {
    (0 until n).foldLeft[(D[A], List[A])]((dist, Nil)){ case ((utdDist1, acc), _) =>
      val (utdDist2, s) = sample(utdDist1)
      (utdDist2, s :: acc)
    }
  }

  def sampling[A](probability: (A, A) => Option[Double], domains: List[RangeM[A]]): Option[DensityPlot] = {
    val recordsO = domains.filter(range => !range.isPoint)
      .traverse(range =>
        probability(range.start, range.end).map { prob =>
          (RangeP.forRangeM(range), (prob / range.length).toDouble)
        }
      )

    for {
      records <- recordsO
    } yield DensityPlot.disjoint(records)
  }

  def samplingForDomain[A](dist: D[A], domains: List[RangeM[A]]): Option[DensityPlot] = {
    sampling((start: A, end: A) => probability(dist, start, end), domains)
  }

  def samplingDist[A](dist: D[A], domains: List[RangeM[A]]): Option[PlottedDist[A]] = for {
    plot <- samplingForDomain(dist, domains)
    conf = SamplingDistConf.forDistConf(dist.conf)
  } yield PlottedDist.bare(dist.measure.asInstanceOf[Measure[A]], plot, conf)

  def samplingDistForPlottedDist[A](dist: D[A],
                                    pltDist: PlottedDist[A]): Option[PlottedDist[A]] = for {
    densityPlot <- Option(pltDist.sampling)
    domainsP = densityPlot.records.map(_._1)
    domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(pltDist.measure))
    dist <- samplingDist(dist, domainsM)
  } yield dist

  def samplingDistForSamplingDist[A](dist: D[A], smplDist: SamplingDist[A]): Option[PlottedDist[A]] = for {
    densityPlot <- smplDist.sampling
    domainsP = densityPlot.records.map(_._1)
    domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(smplDist.measure))
    dist <- samplingDist(dist, domainsM)
  } yield dist

  def uniformSampling[A](dist: D[A], start: A, end: A, size: Int): Option[PlottedDist[A]] = {
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

  def probability[A](dist: Dist[A], start: A, end: A): Option[Double] = dist match {
    case smooth: SmoothDist[A] => SmoothDist.probability(smooth, start, end)
    case sampling: SamplingDist[A] => SamplingDist.probability(sampling, start, end)
    case combination: CombinationDist[A] => CombinationDist.probability(combination, start, end)
  }

  def sample[A](dist: Dist[A]): (Dist[A], A) = dist match {
    case smooth: SmoothDist[A] => SmoothDist.sample(smooth)
    case sampling: SamplingDist[A] => SamplingDist.sample(sampling)
    case combination: CombinationDist[A] => CombinationDist.sample(combination)
  }

  override def pdf[A](dist: Dist[A], a: A): Option[Double] = dist match {
    case smooth: SmoothDist[A] => SmoothDist.pdf(smooth, a)
    case sampling: SamplingDist[A] => SamplingDist.pdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.pdf(combination, a)
    case _ => super.pdf(dist, a)
  }

  override def cdf[A](dist: Dist[A], a: A): Option[Prim] = dist match {
    case smooth: SmoothDist[A] => SmoothDist.cdf(smooth, a)
    case sampling: SamplingDist[A] => SamplingDist.cdf(sampling, a)
    case combination: CombinationDist[A] => CombinationDist.cdf(combination, a)
    case _ => super.cdf(dist, a)
  }

}


