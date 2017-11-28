package sketch.scope.pdf.monad

import sketch.scope.pdf.{Dist, Sketch}

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch] {

  def flatMap[A, B](dist: Sketch[A], f: A => Dist[B]): Sketch[B] = ???

}
