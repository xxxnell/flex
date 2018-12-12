package flex.pdf

import flex.conf.pdf.SmoothDistConf
import flex.measure.Measure
import flex.rand._
import org.apache.commons.math3.distribution.NormalDistribution

/**
  * Normal distribution.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Normal_distribution">Normal distribution - Wikipedia</a>
  * */
trait NormalDist[A] extends NumericDist[A] {
  val mean: Prim
  val variance: Prim
}

trait NormalDistOps extends NumericDistOps[NormalDist] {

  override def pdf[A](dist: NormalDist[A], a: A): Prim = {
    val numericDist = new NormalDistribution(dist.mean, dist.variance)
    val prim = dist.measure.to(a)

    numericDist.density(prim)
  }

  override def cdf[A](dist: NormalDist[A], a: A): Double = {
    val p = dist.measure.to(a)
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    numericDist.cumulativeProbability(p)
  }

  override def icdf[A](dist: NormalDist[A], p: Double): A = {
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    dist.measure.from(numericDist.inverseCumulativeProbability(p))
  }

}

object NormalDist extends NormalDistOps {

  private case class NormalDistImpl[A](measure: Measure[A], conf: SmoothDistConf, mean: Prim, variance: Prim, rng: IRng)
      extends NormalDist[A]

  def apply[A](measure: Measure[A], conf: SmoothDistConf, mean: Prim, variance: Prim): NormalDist[A] =
    bare(measure, conf, mean, variance, IRng(mean.hashCode() + variance.hashCode()))

  def apply[A](mean: A, variance: Prim)(implicit measure: Measure[A], conf: SmoothDistConf): NormalDist[A] =
    apply(measure, conf, measure.to(mean), variance)

  def apply[A](mean: A, variance: Prim, rng: IRng)(implicit measure: Measure[A], conf: SmoothDistConf): NormalDist[A] =
    bare(measure, conf, measure.to(mean), variance, rng)

  def bare[A](measure: Measure[A], conf: SmoothDistConf, mean: Prim, variance: Prim, rng: IRng): NormalDist[A] =
    NormalDistImpl(measure, conf, mean, variance, rng)

  def std(implicit measure: Measure[Double], conf: SmoothDistConf): NormalDist[Double] = apply(0.0, 1.0)

  def modifyRng[A](dist: NormalDist[A], f: IRng => IRng): NormalDist[A] =
    bare(dist.measure, dist.conf, dist.mean, dist.variance, f(dist.rng))

}
