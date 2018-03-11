package flip.pdf.monad

import flip.conf.{DistConf, SamplingDistConf, SketchConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.{DeltaDist, Dist, Prim, SamplingDist, Sketch}

import scala.language.higherKinds

trait DistMonad[D1[_] <: Dist[_], D2[_] <: Dist[_], D3[_] <: Dist[_]]
    extends DistMonadLaws[D1, D2, D3]
    with DistFunctor[D1] {

  def pure[A](a: A, measure: Measure[A]): Dist[A] = {
    val conf = SmoothDistConf.default
    DeltaDist(measure, conf, a)
  }

  def bind[A, B](dist: D1[A], f: A => D2[B], measureB: Measure[B]): D3[B]

}

trait DistMonadLaws[D1[_] <: Dist[_], D2[_] <: Dist[_], D3[_] <: Dist[_]] { self: DistMonad[D1, D2, D3] =>

//  def map[A, B](dist: D1[A], f: A => B, measureB: Measure[B]): D1[B] =
//    bind(dist, (a: A) => pure(f(a), measureB), measureB)

}

object DistMonad {

  def dist: DistMonad[Dist, Dist, Dist] = ???

  def samplingDist: DistMonad[Dist, SamplingDist, SamplingDist] = ???

  def sketch: DistMonad[Dist, Sketch, Sketch] = ???

}
