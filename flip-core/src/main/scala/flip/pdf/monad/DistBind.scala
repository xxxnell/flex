package flip.pdf.monad

import flip.conf.{DistConfB, SamplingDistConf, SamplingDistConfB, SketchConf}
import flip.measure.Measure
import flip.pdf.{Dist, SamplingDist, Sketch}

import scala.language.higherKinds

trait DistBind[D1[_] <: Dist[_], D2[_] <: Dist[_], D3[_] <: SamplingDist[_], C <: SamplingDistConfB[D3[_]]] {

  def bind[A, B](dist: D1[A], f: A => D2[B], measureB: Measure[B], conf: C): D3[B]

}

object DistBind {

    def dist: DistBind[Dist, Dist, SamplingDist, SamplingDistConf] = ???

    def samplingDist: DistBind[Dist, SamplingDist, SamplingDist, SamplingDistConf] = ???

    def sketch: DistBind[Dist, Sketch, Sketch, SketchConf] = ???

}