package flip.pdf

import flip.measure.Measure
import flip.rand.IRng
import org.apache.commons.math3.distribution.LogNormalDistribution

case class LogNormalDist[A](measure: Measure[A], scale: A, shape: Prim, rng: IRng) extends NumericDist[A]

trait LogNormalDistOps extends NumericDistOps[LogNormalDist] {

  def pdf[A](dist: LogNormalDist[A], a: A): Option[Double] = {
    val scale = dist.measure.to(dist.scale)
    val p = dist.measure.to(a)
    val numeric = new LogNormalDistribution(scale, dist.shape)

    Some(numeric.density(p))
  }

  def cdf[A](dist: LogNormalDist[A], a: A): Double = {
    val scale = dist.measure.to(dist.scale)
    val p = dist.measure.to(a)
    val numeric = new LogNormalDistribution(scale, dist.shape)

    numeric.cumulativeProbability(p)
  }

  def icdf[A](dist: LogNormalDist[A], p: Double): A = {
    val scale = dist.measure.to(dist.scale)
    val numeric = new LogNormalDistribution(scale, dist.shape)

    dist.measure.from(numeric.inverseCumulativeProbability(p))
  }

}

object LogNormalDist extends LogNormalDistOps {

  def apply[A](scale: A, shape: Prim)(implicit measure: Measure[A]): LogNormalDist[A] =
    LogNormalDist(measure, scale, shape, IRng(scale.hashCode() + shape.toInt))

  def modifyRng[A](dist: LogNormalDist[A], f: IRng => IRng): LogNormalDist[A] =
    LogNormalDist(dist.measure, dist.scale, dist.shape, f(dist.rng))

}