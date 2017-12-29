package sketch.scope.sim

import sketch.scope.pdf.{Dist, SampledDist}
import sketch.scope.plot._

import scala.math._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Kullback–Leibler divergence
  * https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  */
object KLD {

  def apply[A](d1: SampledDist[A], d2: SampledDist[A]): Option[Double] = kldForSampling(d1, d2)

  def kldForSampling[A](d1: SampledDist[A], d2: SampledDist[A]): Option[Double] = for {
    density <- kldDensityForSampling(d1, d2)
  } yield density.integral(Double.MinValue, Double.MaxValue)

  def kldDensityForSampling[A](d1: SampledDist[A], d2: SampledDist[A]): Option[DensityPlot] = for {
    plot1 <- d1.densityPlot
    plot2 <- d2.densityPlot
    kldDensityPlot = plot1.modify { case (range, value) => point(value, plot2.interpolation(range.middle)) }
  } yield kldDensityPlot

  def point(value1: Double, value2: Double): Double = (value1, value2) match {
    case (0, _) => 0
    case (_, 0) => Double.PositiveInfinity
    case (_, _) => value1 * log(value1 / value2)
  }

}
