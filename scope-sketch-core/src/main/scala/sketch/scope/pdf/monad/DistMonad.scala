package sketch.scope.pdf.monad

import sketch.scope.conf.{DistConf, SamplingDistConf, SketchConf}
import sketch.scope.measure.Measure
import sketch.scope.pdf.{DeltaDist, Dist, Prim, SamplingDist, Sketch}

import scala.language.higherKinds

trait DistMonad[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_], C<:DistConf]
  extends DistMonadLaws[D1, D2, D3, C]
    with DistFunctor[D1, C] {

  def pure[A](a: A, measure: Measure[A]): Dist[A] = DeltaDist(measure, measure(a))

  def bind[A, B](dist: D1[A], f: A => D2[B], measureB: Measure[B], conf: C): D3[B]

}

trait DistMonadLaws[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_], C<:DistConf] { self: DistMonad[D1, D2, D3, C] =>

//  def map[A, B](dist: D1[A], f: A => B, measureB: Measure[B]): D1[B] =
//    bind(dist, (a: A) => pure(f(a), measureB), measureB)

}

object DistMonad {

  def apply: DistMonad[Dist, Dist, Dist, DistConf] = dist

  def dist: DistMonad[Dist, Dist, Dist, DistConf] = ???

  def samplingDist: DistMonad[Dist, SamplingDist, SamplingDist, SamplingDistConf] = ???

  def sketch: DistMonad[Dist, Sketch, Sketch, SketchConf] = ???

}