package sketch.scope.pdf.monad

import sketch.scope.measure.Measure
import sketch.scope.pdf.{DeltaDist, Dist, Prim, SampleDist, Sketch}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistMonad[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_]]
  extends DistMonadLaws[D1, D2, D3]
    with DistFunctor[D1] {

  def pure[A](a: A, measure: Measure[A]): Dist[A] = DeltaDist(measure, measure(a))

  def bind[A, B](dist: D1[A], f: A => D2[B], measureB: Measure[B]): D3[B]

}

trait DistMonadLaws[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_]] { self: DistMonad[D1, D2, D3] =>

//  def map[A, B](dist: D1[A], f: A => B, measureB: Measure[B]): D1[B] =
//    bind(dist, (a: A) => pure(f(a), measureB), measureB)

}

object DistMonad {

  def apply: DistMonad[Dist, Dist, Dist] = dist

  def dist: DistMonad[Dist, Dist, Dist] = ???

  def sampleDist: DistMonad[Dist, SampleDist, SampleDist] = ???

  def sketch: DistMonad[Dist, Sketch, Sketch] = ???

}