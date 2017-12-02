package sketch.scope.pdf.monad

import sketch.scope.pdf.Dist

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistBind[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_]] {

  def bind[A, B](dist: D1[A], f: A => D2[B]): D3[B]

}
