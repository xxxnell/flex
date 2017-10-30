package sketch.scope.sketch.algebra

import sketch.scope.sketch.{DeltaDist, Dist, Prim}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistMonad[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_]] {

  def pure[A](measure: A => Prim, a: A): Dist[A] = DeltaDist(measure, measure(a))

  def map[A, B](dist: D1[A], f: A => B): D1[B]

  def flatMap[A, B](dist: D1[A], f: A => D2[B]): D3[B]

}
