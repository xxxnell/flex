package sketch.scope.pdf.monad

import sketch.scope.pdf.{Dist, Sketch}

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSketchMonad extends SketchMonad[Sketch, Dist, Sketch] {

  override def map[A, B](dist: Sketch[A], f: A => B): Sketch[B] = ???

  override def bind[A, B](dist: Sketch[A], f: A => Dist[B]): Sketch[B] = PointToPointSketchBind.bind(dist, f)

}
