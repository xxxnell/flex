package sketch.scope.pdf.monad

import sketch.scope.pdf.Dist

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistFunctor[D1[_]<:Dist[_]] {

  def map[A, B](dist: D1[A], f: A => B): D1[B]

}
