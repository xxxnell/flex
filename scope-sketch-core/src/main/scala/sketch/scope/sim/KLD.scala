package sketch.scope.sim

import sketch.scope.conf.DistConf
import sketch.scope.pdf.{Dist, SamplingDist}
import sketch.scope.plot._
import sketch.scope.pdf.syntax._

import scala.math._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Kullback–Leibler divergence
  * https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  */
object KLD {

  def apply[A](d1: SamplingDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[Double] =
    kldForSampling(d1, d2, conf2)

  def point(value1: Double, value2: Double): Double = (value1, value2) match {
    case (0, _) => 0
    case (_, 0) => Double.PositiveInfinity
    case (_, _) => value1 * log(value1 / value2)
  }

  def kldForSampling[A](d1: SamplingDist[A], d2: Dist[A], conf2: DistConf): Option[Double] = for {
    density <- kldDensityForSampling(d1, d2, conf2)
  } yield density.integral(Double.MinValue, Double.MaxValue)

  def kldDensityForSampling[A](d1: SamplingDist[A], d2: Dist[A], conf2: DistConf): Option[DensityPlot] = for {
    plot1 <- d1.densityPlot
    // todo handle the case if the d2.pdf doesn't exists.
    kldDensityPlot = plot1.modify { case (range, value) =>
      point(value, d2.pdf(d2.measure.from(range.middle))(conf2).getOrElse(0))
    }
  } yield kldDensityPlot

}

trait KLDSyntax {

  private val kld = sketch.scope.sim.KLD

  def KLD[A](d1: SamplingDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[Double] =
    kld.kldForSampling(d1, d2, conf2)

  def KLDDensity[A](d1: SamplingDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[DensityPlot] =
    kld.kldDensityForSampling(d1, d2, conf2)

}