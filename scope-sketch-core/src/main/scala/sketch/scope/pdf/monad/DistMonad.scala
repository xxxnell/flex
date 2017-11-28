package sketch.scope.pdf.monad

import sketch.scope.pdf.{DeltaDist, Dist, Prim, SampleDist, Sketch}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistMonad[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_]] extends DistFunctor[D1] {

  def pure[A](measure: A => Prim, a: A): Dist[A] = DeltaDist(measure, measure(a))

  def bind[A, B](dist: D1[A], f: A => D2[B]): D3[B]

}

object DistMonad {

  def apply: DistMonad[Dist, Dist, Dist] = dist

  def dist: DistMonad[Dist, Dist, Dist] = ???

  def sampleDist: DistMonad[Dist, SampleDist, SampleDist] = ???

  def sketch: DistMonad[Dist, Sketch, Sketch] = ???

}