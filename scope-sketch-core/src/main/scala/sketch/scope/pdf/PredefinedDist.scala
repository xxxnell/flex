package sketch.scope.pdf

import sketch.scope.conf.SmoothDistConf
import sketch.scope.measure.Measure

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait PredefinedDist[A] extends SmoothDist[A] {

  def probability(from: A, to: A): Option[Double]

}

trait PredefinedDistOps[D[_]<:PredefinedDist[_]] extends SmoothDistPropOps[D, SmoothDistConf] {

  def probability[A](dist: D[A], from: A, to: A): Option[Double] =
    dist.asInstanceOf[PredefinedDist[A]].probability(from, to)

}

object PredefinedDist extends PredefinedDistOps[PredefinedDist] {

  case class PredefinedDistImpl[A](measure: Measure[A],
                                   probabilityF: (A, A) => Option[Double]) extends PredefinedDist[A] {
    def probability(from: A, to: A): Option[Prim] = probabilityF(from, to)
  }

  def apply[A](measure: Measure[A], probability: (A, A) => Option[Double]): PredefinedDist[A] =
    PredefinedDistImpl(measure, probability)

  def sample[A](dist: PredefinedDist[A]): (PredefinedDist[A], A) = ???

  def pdf[A](dist: PredefinedDist[A], a: A): Option[Prim] = ???

}


