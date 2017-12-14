package sketch.scope.pdf.monad

import sketch.scope.measure.Measure
import sketch.scope.pdf.Dist

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistFunctor[D1[_]<:Dist[_]] {

  def map[A, B](dist: D1[A], f: A => B, measureB: Measure[B]): D1[B]

}
