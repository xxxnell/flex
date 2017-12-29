package sketch.scope.sim

import sketch.scope.pdf.{Dist, SampledDist, SmoothDist}
import sketch.scope.plot._

import scala.math._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Kullback–Leibler divergence
  * https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  */
object KLD {

  def apply[A](d1: SampledDist[A], d2: Dist[A]): Option[Double] = kldForSampling(d1, d2)

  def point(value1: Double, value2: Double): Double = (value1, value2) match {
    case (0, _) => 0
    case (_, 0) => Double.PositiveInfinity
    case (_, _) => value1 * log(value1 / value2)
  }

  def kldForSampling[A](d1: SampledDist[A], d2: Dist[A]): Option[Double] = for {
    density <- kldDensityForSampling(d1, d2)
  } yield density.integral(Double.MinValue, Double.MaxValue)

  def kldDensityForSampling[A](d1: SampledDist[A], d2: Dist[A]): Option[DensityPlot] = for {
    plot1 <- d1.densityPlot
    // todo handle the case if the d2.pdf doesn't exists.
    kldDensityPlot = plot1.modify { case (range, value) =>
      point(value, d2.pdf(d2.measure.from(range.middle)).getOrElse(0))
    }
  } yield kldDensityPlot


}
