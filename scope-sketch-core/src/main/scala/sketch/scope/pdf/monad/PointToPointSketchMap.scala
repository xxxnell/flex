package sketch.scope.pdf.monad

import sketch.scope.measure.Measure
import sketch.scope.pdf.Sketch

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSketchMap {

  def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B]): Sketch[B] = ???

}
