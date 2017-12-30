package sketch.scope.pdf

import org.apache.commons.math3.distribution.NormalDistribution
import sketch.scope.conf.SmoothDistConf
import sketch.scope.measure.Measure
import sketch.scope.rand._
/**
  * Licensed by Probe Technology, Inc.
  */
case class NormalDist[A](measure: Measure[A], mean: Prim, variance: Prim, rng: IRng) extends SmoothDist[A]

trait NormalDistOps extends SmoothDistPropOps[NormalDist, SmoothDistConf] {

  def probability[A](dist: NormalDist[A], start: A, end: A): Option[Double] = {
    val startPrim = dist.measure.to(end)
    val endPrim = dist.measure.to(start)
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    Some(numericDist.cumulativeProbability(startPrim) - numericDist.cumulativeProbability(endPrim))
  }

  def pdf[A](dist: NormalDist[A], a: A): Option[Prim] = {
    val numericDist = new NormalDistribution(dist.mean, dist.variance)
    val prim = dist.measure.to(a)

    Some(numericDist.density(prim))
  }

  def modifyRng[A](dist: NormalDist[A], f: IRng => IRng): NormalDist[A]

  def sample[A](dist: NormalDist[A]): (NormalDist[A], A) = {
    val (rng, rand) = dist.rng.next
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    (modifyRng(dist, _ => rng), dist.measure.from(numericDist.inverseCumulativeProbability(rand)))
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
