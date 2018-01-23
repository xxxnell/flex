package flip.pdf

import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.{RangeM, RangeP}

import scala.language.higherKinds
import cats.implicits._
import flip.conf.{SamplingDistConf, SmoothDistConf}

trait SmoothDist[A] extends Dist[A]

trait SmoothDistPropOps[D[_]<:SmoothDist[_], C<:SmoothDistConf]
  extends DistPropOps[D, C]
    with SmoothDistPropLaws[D, C] {

  def pdf[A](dist: D[A], a: A): Option[Double]

  def probability[A](dist: D[A], start: A, end: A): Option[Prim]

}

trait SmoothDistPropLaws[D[_]<:SmoothDist[_], C<:SmoothDistConf] { self: SmoothDistPropOps[D, C] =>

  def probability[A](dist: D[A], start: A, end: A, conf: SmoothDistConf): Option[Prim] =
    probability(dist, start, end)

  def samplingForDomain[A](dist: D[A], domains: List[RangeM[A]]): Option[DensityPlot] = {
    sampling((start: A, end: A) => probability(dist, start, end), domains)
  }

  def samplingDist[A](dist: D[A], domains: List[RangeM[A]]): Option[PlottedDist[A]] = for {
    plot <- samplingForDomain(dist, domains)
  } yield PlottedDist(dist.measure.asInstanceOf[Measure[A]], plot)

  def samplingDistForSamplingDist[A](dist: D[A],
                                     smplDist: SamplingDist[A],
                                     smplConf: SamplingDistConf): Option[PlottedDist[A]] = for {
    densityPlot <- smplDist.sampling(smplConf)
    domainsP = densityPlot.records.map(_._1)
    domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(smplDist.measure))
    dist <- samplingDist(dist, domainsM)
  } yield dist

  def uniformSampling[A](dist: D[A], start: A, end: A, size: Int): Option[PlottedDist[A]] = {
    val domains = RangeM(start, end)(dist.measure.asInstanceOf[Measure[A]]).uniformSplit(size)

    samplingDist(dist, domains)
  }

}

object SmoothDist extends SmoothDistPropOps[SmoothDist, SmoothDistConf] {

  def probability[A](dist: SmoothDist[A], start: A, end: A): Option[Prim] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.probability(predefined, start, end)
    case numeric: NumericDist[A] => NumericDist.probability(numeric, start, end)
  }

  def sample[A](dist: SmoothDist[A]): (SmoothDist[A], A) = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.sample(predefined)
    case numeric: NumericDist[A] => NumericDist.sample(numeric)
  }

  def pdf[A](dist: SmoothDist[A], a: A): Option[Double] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.pdf(predefined, a)
    case numeric: NumericDist[A] => NumericDist.pdf(numeric, a)
  }

}