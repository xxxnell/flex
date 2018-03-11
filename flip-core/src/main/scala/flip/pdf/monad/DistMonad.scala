package flip.pdf.monad

import flip.conf._
import flip.measure.Measure
import flip.pdf.{DeltaDist, Dist, Prim, SamplingDist, Sketch}

import scala.language.higherKinds

/**
  * Giry monad.
  * */
trait DistMonad[D1[_] <: Dist[_], D2[_] <: Dist[_], D3[_] <: Dist[_], C <: DistConfB[D3[_]]]
    extends DistMonadLaws[D1, D2, D3, C]
    with DistFunctor[D3, C] {

  def pure[A](a: A, measure: Measure[A]): Dist[A] = {
    val conf = SmoothDistConf.default
    DeltaDist(measure, conf, a)
  }

  def bind[A, B](dist: D1[A], f: A => D2[B], measureB: Measure[B], conf: C): D3[B]

}

trait DistMonadLaws[D1[_] <: Dist[_], D2[_] <: Dist[_], D3[_] <: Dist[_], C <: DistConfB[D3[_]]] {
  self: DistMonad[D1, D2, D3, C] =>

//  def map[A, B](dist: D1[A], f: A => B, measureB: Measure[B]): D1[B] =
//    bind(dist, (a: A) => pure(f(a), measureB), measureB)

}

object DistMonad {

  def dist: DistMonad[Dist, Dist, Dist, DistConf] = ???

  def samplingDist: DistMonad[Dist, SamplingDist, SamplingDist, SamplingDistConf] = ???

  def sketch: DistMonad[Dist, Sketch, Sketch, SketchConf] = ???

}
