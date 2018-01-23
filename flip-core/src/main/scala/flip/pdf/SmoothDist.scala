package flip.pdf

import flip.conf.SmoothDistConf

import scala.language.higherKinds

trait SmoothDist[A] extends Dist[A] {

  def conf: SmoothDistConf

}

trait SmoothDistPropOps[D[_]<:SmoothDist[_]]
  extends DistPropOps[D]
    with SmoothDistPropLaws[D] {

  def pdf[A](dist: D[A], a: A): Option[Double]

  def probability[A](dist: D[A], start: A, end: A): Option[Prim]

}

trait SmoothDistPropLaws[D[_]<:SmoothDist[_]] { self: SmoothDistPropOps[D] =>

}

object SmoothDist extends SmoothDistPropOps[SmoothDist] {

  def probability[A](dist: SmoothDist[A], start: A, end: A): Option[Prim] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.probability(predefined, start, end)
    case numeric: NumericDist[A] => NumericDist.probability(numeric, start, end)
  }

  def sample[A](dist: SmoothDist[A]): (SmoothDist[A], A) = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.sample(predefined)
    case numeric: NumericDist[A] => NumericDist.sample(numeric)
  }

  override def pdf[A](dist: SmoothDist[A], a: A): Option[Double] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.pdf(predefined, a)
    case numeric: NumericDist[A] => NumericDist.pdf(numeric, a)
    case _ => super.pdf(dist, a)
  }

}