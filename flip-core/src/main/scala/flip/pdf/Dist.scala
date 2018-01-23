package flip.pdf

import flip.conf.{DistConf, SamplingDistConf}
import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.{RangeM, RangeP}

import scala.language.higherKinds
import cats.implicits._

/**
  * Distribution, or probability bistribution provides to get probability for given domain.
  * */
trait Dist[A] {

  def measure: Measure[A]

}

trait DistPropOps[D[_]<:Dist[_], C<:DistConf] extends DistPropLaws[D, C] {

  def probability[A](dist: D[A], from: A, to: A, conf: C): Option[Double]

  def sample[A](dist: D[A]): (D[A], A)

}

trait DistPropLaws[D[_]<:Dist[_], C<:DistConf] { self: DistPropOps[D, C] =>

  def numericPdf[A](dist: D[A], a: A, conf: C): Option[Double] = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val ap = measure.from(measure.to(a) + conf.delta)

    probability(dist, a, ap, conf).map(prob => prob / conf.delta)
  }

  def pdf[A](dist: D[A], a: A, conf: C): Option[Double] = ???

  def cdf[A](sketch: D[A], a: A, conf: C): Option[Double] = ???

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

  def samplingForDomain[A](dist: D[A], domains: List[RangeM[A]], conf: C): Option[DensityPlot] = {
    sampling((start: A, end: A) => probability(dist, start, end, conf), domains)
  }

  def samplingDist[A](dist: D[A], domains: List[RangeM[A]], conf: C): Option[PlottedDist[A]] = for {
    plot <- samplingForDomain(dist, domains, conf)
  } yield PlottedDist(dist.measure.asInstanceOf[Measure[A]], plot)

  def samplingDistForPlottedDist[A](dist: D[A],
                                    conf: C,
                                    pltDist: PlottedDist[A]): Option[PlottedDist[A]] = for {
    densityPlot <- Option(pltDist.sampling)
    domainsP = densityPlot.records.map(_._1)
    domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(pltDist.measure))
    dist <- samplingDist(dist, domainsM, conf)
  } yield dist

  def samplingDistForSamplingDist[A](dist: D[A],
                                     conf: C,
                                     smplDist: SamplingDist[A],
                                     smplDistconf: SamplingDistConf): Option[PlottedDist[A]] = for {
    densityPlot <- smplDist.sampling(smplDistconf)
    domainsP = densityPlot.records.map(_._1)
    domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(smplDist.measure))
    dist <- samplingDist(dist, domainsM, conf)
  } yield dist

}

object Dist extends DistPropOps[Dist, DistConf] { self =>

  def delta[A](center: A)(implicit measure: Measure[A]): Dist[A] = DeltaDist(measure, center)

  def normal[A](mean: A, variance: Double)(implicit measure: Measure[A]): NormalDist[A] =
    NormalDist(mean, variance)

  // pipelining

  def probability[A](dist: Dist[A], from: A, to: A, conf: DistConf): Option[Double] = (dist, conf) match {
    case (smooth: SmoothDist[A], _) => SmoothDist.probability(smooth, from, to)
    case (sampling: SamplingDist[A], conf: SamplingDistConf) => SamplingDist.probability(sampling, from, to, conf)
  }

  def sample[A](dist: Dist[A]): (Dist[A], A) = dist match {
    case smooth: SmoothDist[A] => SmoothDist.sample(smooth)
    case sampling: SamplingDist[A] => SamplingDist.sample(sampling)
  }

  override def pdf[A](dist: Dist[A], a: A, conf: DistConf): Option[Double] = (dist, conf) match {
    case (smooth: SmoothDist[A], _) => SmoothDist.pdf(smooth, a)
    case (sampling: SamplingDist[A], conf: SamplingDistConf) => SamplingDist.pdf(sampling, a, conf)
    case _ => super.pdf(dist, a, conf)
  }

}


