package flip.pdf

import flip.measure.Measure
import flip.rand.IRng
import org.apache.commons.math3.distribution.ParetoDistribution

case class ParetoDist[A](measure: Measure[A], scale: A, shape: Prim, rng: IRng) extends NumericDist[A]

trait ParetoDistOps extends NumericDistOps[ParetoDist] {

  def pdf[A](dist: ParetoDist[A], a: A): Option[Prim] = {
    val scale = dist.measure.to(dist.scale)
    val p = dist.measure.to(a)
    val numeric = new ParetoDistribution(scale, dist.shape)

    Some(numeric.density(p))
  }

  def cdf[A](dist: ParetoDist[A], a: A): Prim = {
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

  def apply[A](scale: A, shape: Double)(implicit measure: Measure[A]): ParetoDist[A] =
    ParetoDist(measure, scale, shape, IRng(scale.hashCode() + shape.toInt))

  def modifyRng[A](dist: ParetoDist[A], f: IRng => IRng): ParetoDist[A] =
    ParetoDist(dist.measure, dist.scale, dist.shape, f(dist.rng))

}


