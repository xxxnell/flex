package flip.pdf

import flip.measure.Measure
import flip.rand._
import org.apache.commons.math3.distribution.NormalDistribution

/**
  * Normal distribution.
  * */
case class NormalDist[A](measure: Measure[A], mean: Prim, variance: Prim, rng: IRng) extends NumericDist[A]

trait NormalDistOps extends NumericDistOps[NormalDist] {

  def pdf[A](dist: NormalDist[A], a: A): Option[Prim] = {
    val numericDist = new NormalDistribution(dist.mean, dist.variance)
    val prim = dist.measure.to(a)

    Some(numericDist.density(prim))
  }

  def cdf[A](dist: NormalDist[A], a: A): Double = {
    val p = dist.measure.to(a)
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    numericDist.cumulativeProbability(p)
  }

  def icdf[A](dist: NormalDist[A], p: Double): A = {
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    dist.measure.from(numericDist.inverseCumulativeProbability(p))
  }

}

object NormalDist extends NormalDistOps {

  def apply[A](measure: Measure[A], mean: Prim, variance: Prim): NormalDist[A] =
    bare(measure, mean, variance, IRng(mean.toInt + variance.toInt))

  def bare[A](measure: Measure[A], mean: Prim, variance: Prim, rng: IRng): NormalDist[A] =
    NormalDist(measure, mean, variance, rng)

  def apply[A](mean: A, variance: Prim)(implicit measure: Measure[A]): NormalDist[A] =
    apply(measure, measure.to(mean), variance)

  def modifyRng[A](dist: NormalDist[A], f: IRng => IRng): NormalDist[A] =
    NormalDist(dist.measure, dist.mean, dist.variance, f(dist.rng))

}
