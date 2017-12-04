package sketch.scope.pdf.monad

import sketch.scope.measure.Measure
import sketch.scope.pdf.{Dist, Sketch}

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSketchMonad extends SketchMonad[Sketch, Dist, Sketch] {

  def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B]): Sketch[B] = ???

  def bind[A, B](dist: Sketch[A], f: A => Dist[B], measureB: Measure[B]): Sketch[B] =
    PointToPointSketchBind.bind(dist, f, measureB)

}
