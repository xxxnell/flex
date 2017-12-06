package sketch.scope.rand

import cats.data.State

/**
  * Licensed by Probe Technology, Inc.
  *
  * Random Generator
  */
trait Rng[S, A] {

  def seed: S

}

trait RngOps[S, A, R[_<:S, _<:A]<:Rng[_, _]] {

  def nextS: State[R[S, A], A]

}

object Rng {

  def interval(seed: Int): IRng = IRng(seed)

}

