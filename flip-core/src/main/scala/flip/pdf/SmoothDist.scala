package flip.pdf

import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.{RangeM, RangeP}

import scala.language.higherKinds
import cats.implicits._
import flip.conf.SmoothDistConf

trait SmoothDist[A] extends Dist[A]

trait SmoothDistPropOps[D[_]<:SmoothDist[_], C<:SmoothDistConf]
  extends DistPropOps[D, C]
    with SmoothDistPropLaws[D, C] {

  def pdf[A](dist: D[A], a: A): Option[Double]

  def probability[A](dist: D[A], from: A, to: A): Option[Prim]

}

trait SmoothDistPropLaws[D[_]<:SmoothDist[_], C<:SmoothDistConf] { self: SmoothDistPropOps[D, C] =>

  def probability[A](dist: D[A], from: A, to: A, conf: SmoothDistConf): Option[Prim] =
    probability(dist, from, to)

  def sampling[A](dist: D[A], domains: List[RangeM[A]]): Option[DensityPlot] = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val recordsO = domains.filter(range => !range.isPoint)
      .traverse(range =>
        probability(dist, range.start, range.end).map { prob =>
          (RangeP.forRangeM(range), (prob / range.length).toDouble)
        }
      )

    for {
      records <- recordsO
    } yield DensityPlot.disjoint(records)
  }

  def toSamplingDist[A](dist: D[A], domains: List[RangeM[A]]): Option[PlottedDist[A]] = {
    val plotO = sampling(dist, domains)

    for {
      plot <- plotO
    } yield PlottedDist(dist.measure.asInstanceOf[Measure[A]], plot)
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