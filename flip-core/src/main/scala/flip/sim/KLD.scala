package flip.sim

import flip.conf.DistConf
import flip.pdf.{Dist, SamplingDist}
import flip.plot._
import flip.pdf.syntax._

import scala.math._

/**
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
    domain <- density.domain
  } yield density.integral(domain.start, domain.end)

  def kldDensityForSampling[A](d1: SamplingDist[A], d2: Dist[A], conf2: DistConf): Option[DensityPlot] = for {
    plot1 <- d1.densityPlot
    // todo handle the case if the d2.pdf doesn't exists.
    kldDensityPlot = plot1.modify { case (range, value) =>
      point(value, d2.pdf(d2.measure.from(range.middle))(conf2).getOrElse(0))
    }
  } yield kldDensityPlot

}
