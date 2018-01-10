package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.rand.IRng
import org.apache.commons.math3.distribution.NormalDistribution

import scala.language.higherKinds

trait NumericDist[A] extends SmoothDist[A] {

  def rng: IRng

}

trait NumericDistOps[D[_]<:NumericDist[_]] extends SmoothDistPropOps[D, SmoothDistConf] {

  def modifyRng[A](dist: D[A], f: IRng => IRng): D[A]

  def cdf[A](dist: D[A], a: A): Double

  def icdf[A](dist: D[A], a: Double): A

  def probability[A](dist: D[A], start: A, end: A): Option[Double] = Some(cdf(dist, end) - cdf(dist, start))

  def sample[A](dist: D[A]): (D[A], A) = {
    val (rng, rand) = dist.rng.next

    (modifyRng(dist, _ => rng), icdf(dist, rand))
  }

}

object NumericDist extends NumericDistOps[NumericDist] {

  // constructor

  def delta[A](pole: A)(implicit measure: Measure[A]): DeltaDist[A] =
    DeltaDist(measure, pole)

  def normal[A](mean: A, variance: Double)(implicit measure: Measure[A]): NormalDist[A] =
    NormalDist(mean, variance)

  def logNormal[A](scale: A, shape: Double)(implicit measure: Measure[A]): LogNormalDist[A] =
    LogNormalDist(scale, shape)

  def pareto[A](scale: A, shape: Double)(implicit measure: Measure[A]): ParetoDist[A] =
    ParetoDist(scale, shape)

  // pipelining

  def pdf[A](dist: NumericDist[A], a: A): Option[Prim] = dist match {
    case dist: ParetoDist[A] => ParetoDist.pdf(dist, a)
    case dist: LogNormalDist[A] => LogNormalDist.pdf(dist, a)
    case dist: NormalDist[A] => NormalDist.pdf(dist, a)
    case dist: DeltaDist[A] => DeltaDist.pdf(dist, a)
  }

  def cdf[A](dist: NumericDist[A], a: A): Double = dist match {
    case dist: ParetoDist[A] => ParetoDist.cdf(dist, a)
    case dist: LogNormalDist[A] => LogNormalDist.cdf(dist, a)
    case dist: NormalDist[A] => NormalDist.cdf(dist, a)
    case dist: DeltaDist[A] => DeltaDist.cdf(dist, a)
  }

  def icdf[A](dist: NumericDist[A], p: Double): A = dist match {
    case dist: ParetoDist[A] => ParetoDist.icdf(dist, p)
    case dist: LogNormalDist[A] => LogNormalDist.icdf(dist, p)
    case dist: NormalDist[A] => NormalDist.icdf(dist, p)
    case dist: DeltaDist[A] => DeltaDist.icdf(dist, p)
  }

  def modifyRng[A](dist: NumericDist[A], f: IRng => IRng): NumericDist[A] = dist match {
    case dist: ParetoDist[A] => ParetoDist.modifyRng(dist, f)
    case dist: LogNormalDist[A] => LogNormalDist.modifyRng(dist, f)
    case dist: NormalDist[A] => NormalDist.modifyRng(dist, f)
    case dist: DeltaDist[A] => DeltaDist.modifyRng(dist, f)
  }

}