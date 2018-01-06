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

}

trait SmoothDistPropLaws[D[_]<:SmoothDist[_], C<:SmoothDistConf] { self: SmoothDistPropOps[D, C] =>

  def densityPlot[A](dist: D[A], domains: List[RangeM[A]]): Option[DensityPlot] = {
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
    val plotO = densityPlot(dist, domains)

    for {
      plot <- plotO
    } yield PlottedDist(dist.measure.asInstanceOf[Measure[A]], plot)
  }

}

object SmoothDist extends SmoothDistPropOps[SmoothDist, SmoothDistConf] {

  def probability[A](dist: SmoothDist[A], start: A, end: A): Option[Prim] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.probability(predefined, start, end)
    case delta: DeltaDist[A] => DeltaDist.probability(delta, start, end)
    case normal: NormalDist[A] => NormalDist.probability(normal, start, end)
  }

  def sample[A](dist: SmoothDist[A]): (SmoothDist[A], A) = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.sample(predefined)
    case delta: DeltaDist[A] => DeltaDist.sample(delta)
    case normal: NormalDist[A] => NormalDist.sample(normal)
  }

  def pdf[A](dist: SmoothDist[A], a: A): Option[Double] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.pdf(predefined, a)
    case delta: DeltaDist[A] => DeltaDist.pdf(delta, a)
    case normal: NormalDist[A] => NormalDist.pdf(normal, a)
  }

}