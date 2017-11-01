package sketch.scope.sketch.algebra

import sketch.scope.sketch.{Dist, SmoothDist}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistBind[D1[_]<:SmoothDist[_], D2[_]<:Dist[_], D3[_]<:Dist[_]] extends DistBind[D1, D2, D3] {

  def flatMap[A, B](dist: D1[A], f: A => D2[B]): D3[B]

}
