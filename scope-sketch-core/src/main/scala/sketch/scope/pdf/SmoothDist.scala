package sketch.scope.pdf

import sketch.scope.measure.Measure
import sketch.scope.plot.DensityPlot
import sketch.scope.range.{RangeM, RangeP}

import scala.language.higherKinds
import cats.implicits._

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDist[A] extends Dist[A]

trait SmoothDistPropOps[D[_]<:SmoothDist[_]] extends DistPropOps[D] with SmoothDistPropLaws[D] {

  def pdf[A](dist: D[A], a: A): Option[Double]

}

trait SmoothDistPropLaws[D[_]<:SmoothDist[_]] { self: SmoothDistPropOps[D] =>

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

  def toSampleDist[A](dist: D[A], domains: List[RangeM[A]]): Option[SampledDist[A]] = {
    val plotO = densityPlot(dist, domains)

    for {
      plot <- plotO
    } yield PlottedDist(dist.measure.asInstanceOf[Measure[A]], plot)
  }

}

object SmoothDist extends SmoothDistPropOps[SmoothDist] {

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