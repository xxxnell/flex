package sketch.scope.dist.monad

import sketch.scope.dist.{Dist, Sketch}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchBind[Sketch1[_]<:Sketch[_], D[_]<:Dist[_], Sketch2[_]<:Sketch[_]]
  extends SampleDistBind[Sketch1, D, Sketch2] {

  def flatMap[A, B](dist: Sketch1[A], f: A => D[B]): Sketch2[B]

}
