package sketch.scope.pdf.monad

import sketch.scope.pdf.{Dist, Sketch}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchMonad[Sketch1[_]<:Sketch[_], D[_]<:Dist[_], Sketch2[_]<:Sketch[_]]
  extends SampleDistMonad[Sketch1, D, Sketch2]

object SketchMonad {

  def pointToPoint: SketchMonad[Sketch, Dist, Sketch] = PointToPointSketchMonad

}
