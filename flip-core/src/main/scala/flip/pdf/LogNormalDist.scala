package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.rand.IRng
import org.apache.commons.math3.distribution.LogNormalDistribution

trait LogNormalDist[A] extends NumericDist[A] {
  val scale: A
  val shape: Prim
}

trait LogNormalDistOps extends NumericDistOps[LogNormalDist] {

  override def pdf[A](dist: LogNormalDist[A], a: A): Option[Double] = {
    val scale = dist.measure.to(dist.scale)
    val p = dist.measure.to(a)
    val numeric = new LogNormalDistribution(scale, dist.shape)

    Some(numeric.density(p))
  }

  override def cdf[A](dist: LogNormalDist[A], a: A): Option[Double] = {
    val scale = dist.measure.to(dist.scale)
    val p = dist.measure.to(a)
    val numeric = new LogNormalDistribution(scale, dist.shape)

    Some(numeric.cumulativeProbability(p))
  }

  def icdf[A](dist: LogNormalDist[A], p: Double): A = {
    val scale = dist.measure.to(dist.scale)
    val numeric = new LogNormalDistribution(scale, dist.shape)

    dist.measure.from(numeric.inverseCumulativeProbability(p))
  }

}

object LogNormalDist extends LogNormalDistOps {

  private case class LogNormalDistImpl[A](measure: Measure[A], conf: SmoothDistConf, scale: A, shape: Prim, rng: IRng)
    extends LogNormalDist[A]

  def apply[A](scale: A, shape: Prim)(implicit measure: Measure[A], conf: SmoothDistConf): LogNormalDist[A] =
    bare(measure, conf, scale, shape, IRng(scale.hashCode() + shape.toInt))

  def apply[A](scale: A, shape: Prim, rng: IRng)(implicit measure: Measure[A], conf: SmoothDistConf): LogNormalDist[A] =
    bare(measure, conf, scale, shape, rng)

  def bare[A](measure: Measure[A], conf: SmoothDistConf, scale: A, shape: Prim, rng: IRng): LogNormalDist[A] =
    LogNormalDistImpl(measure, conf, scale, shape, rng)

  def modifyRng[A](dist: LogNormalDist[A], f: IRng => IRng): LogNormalDist[A] =
    bare(dist.measure, dist.conf, dist.scale, dist.shape, f(dist.rng))

}