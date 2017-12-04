package sketch.scope.pdf

import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
trait PredefinedDist[A] extends Dist[A] {

  def probability(from: A, to: A): Option[Double]

}

trait GroomedDistOps[D[_]<:PredefinedDist[_]] extends DistPropOps[D] {

  def probability[A](dist: D[A], from: A, to: A): Option[Double] =
    dist.asInstanceOf[PredefinedDist[A]].probability(from, to)

}

object PredefinedDist extends GroomedDistOps[PredefinedDist] {

  case class PredefinedDistImpl[A](measure: Measure[A],
                                   probabilityF: (A, A) => Option[Double]) extends PredefinedDist[A] {
    def probability(from: A, to: A): Option[Prim] = probabilityF(from, to)
  }

  def apply[A](measure: Measure[A], probability: (A, A) => Option[Double]): PredefinedDist[A] =
    PredefinedDistImpl(measure, probability)

}


