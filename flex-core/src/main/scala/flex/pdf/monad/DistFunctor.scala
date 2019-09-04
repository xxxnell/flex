package flex.pdf.monad

import flex.conf.pdf._
import flex.conf.pdf.SamplingDistConfB
import flex.measure.Measure
import flex.pdf.{ Dist, SamplingDist, Sketch }

import scala.language.higherKinds

trait DistFunctor[D1[_] <: Dist[_], D2[_] <: SamplingDist[_], C <: SamplingDistConfB[D2[_]]] {

  def map[A, B](dist: D1[A], f: A => B, measure: Measure[B], conf: C): D2[B]

}

object DistFunctor {

  def samplingDist: DistFunctor[Dist, SamplingDist, SamplingDistConf] = CombinatorialDistFunctor

//  def sketch: DistFunctor[Dist, Sketch, SketchConf] = ???

}
