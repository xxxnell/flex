package sketch.scope.sketch.algebra

import sketch.scope.sketch.{Dist, Sketch}

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch] {

  def flatMap[A, B](dist: Sketch[A], f: A => Dist[B]): Sketch[B] = ???

}
