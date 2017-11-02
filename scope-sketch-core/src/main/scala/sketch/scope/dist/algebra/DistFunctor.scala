package sketch.scope.dist.algebra

import sketch.scope.dist.Dist

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistFunctor[D1[_]<:Dist[_]] {

  def map[A, B](dist: D1[A], f: A => B): D1[B]

}
