package sketch.scope.dist.algebra

import sketch.scope.dist.{Dist, SmoothDist}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistMonad[D1[_]<:SmoothDist[_], D2[_]<:Dist[_], D3[_]<:Dist[_]] extends DistMonad[D1, D2, D3] {

  def bind[A, B](dist: D1[A], f: A => D2[B]): D3[B]

}
