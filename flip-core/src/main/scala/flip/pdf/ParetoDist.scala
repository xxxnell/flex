package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.rand.IRng
import org.apache.commons.math3.distribution.ParetoDistribution

/**
  * Pareto distribution.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Pareto_distribution">Pareto distribution - Wikipedia</a>
  * */
trait ParetoDist[A] extends NumericDist[A] {
  val scale: A
  val shape: Prim
}

trait ParetoDistOps extends NumericDistOps[ParetoDist] {

  override def pdf[A](dist: ParetoDist[A], a: A): Prim = {
    val scale = dist.measure.to(dist.scale)
    val p = dist.measure.to(a)
    val numeric = new ParetoDistribution(scale, dist.shape)

    numeric.density(p)
  }

  override def cdf[A](dist: ParetoDist[A], a: A): Prim = {
    val scale = dist.measure.to(dist.scale)
    val p = dist.measure.to(a)
    val numeric = new ParetoDistribution(scale, dist.shape)

    numeric.cumulativeProbability(p)
  }

  def icdf[A](dist: ParetoDist[A], p: Double): A = {
    val scale = dist.measure.to(dist.scale)
    val numeric = new ParetoDistribution(scale, dist.shape)

    dist.measure.from(numeric.inverseCumulativeProbability(p))
  }

}

object ParetoDist extends ParetoDistOps {

  private case class ParetoDistImpl[A](measure: Measure[A], conf: SmoothDistConf, scale: A, shape: Prim, rng: IRng)
      extends ParetoDist[A]

  def apply[A](scale: A, shape: Double)(implicit measure: Measure[A], conf: SmoothDistConf): ParetoDist[A] =
    bare(measure, conf, scale, shape, IRng(scale.hashCode() + shape.hashCode))

  def apply[A](scale: A, shape: Double, rng: IRng)(implicit measure: Measure[A], conf: SmoothDistConf): ParetoDist[A] =
    bare(measure, conf, scale, shape, rng)

  def bare[A](measure: Measure[A], conf: SmoothDistConf, scale: A, shape: Prim, rng: IRng): ParetoDist[A] =
    ParetoDistImpl(measure, conf, scale, shape, rng)

  def modifyRng[A](dist: ParetoDist[A], f: IRng => IRng): ParetoDist[A] =
    bare(dist.measure, dist.conf, dist.scale, dist.shape, f(dist.rng))

}
