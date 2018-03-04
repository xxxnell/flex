package flip.pdf

import flip.conf.SmoothDistConf

import scala.language.higherKinds

/**
  * SmoothDist, or Smooth distribution is the opposite of SamplingDist.
  * */
trait SmoothDist[A] extends Dist[A] {

  def conf: SmoothDistConf

}

trait SmoothDistPropOps[D[_] <: SmoothDist[_]] extends DistPropOps[D] with SmoothDistPropLaws[D] {}

trait SmoothDistPropLaws[D[_] <: SmoothDist[_]] { self: SmoothDistPropOps[D] =>

}

object SmoothDist extends SmoothDistPropOps[SmoothDist] {

  def probability[A](dist: SmoothDist[A], start: A, end: A): Prim = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.probability(predefined, start, end)
    case numeric: NumericDist[A] => NumericDist.probability(numeric, start, end)
  }

  def sample[A](dist: SmoothDist[A]): (SmoothDist[A], A) = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.sample(predefined)
    case numeric: NumericDist[A] => NumericDist.sample(numeric)
  }

  override def pdf[A](dist: SmoothDist[A], a: A): Double = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.pdf(predefined, a)
    case numeric: NumericDist[A] => NumericDist.pdf(numeric, a)
    case _ => super.pdf(dist, a)
  }

  override def cdf[A](dist: SmoothDist[A], a: A): Double = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.cdf(predefined, a)
    case numeric: NumericDist[A] => NumericDist.cdf(numeric, a)
    case _ => super.pdf(dist, a)
  }

}
