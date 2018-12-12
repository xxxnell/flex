package flex.chain.monad

import flex.conf.pdf.{SamplingDistConf, SamplingDistConfB, SketchConf}
import flex.pdf.{Dist, SamplingDist, Sketch}

import scala.language.higherKinds

trait SamplingDistBind[
    D1[_] <: SamplingDist[_], D2[_] <: Dist[_], D3[_] <: SamplingDist[_], C <: SamplingDistConfB[D3[_]]]
    extends DistBind[D1, D2, D3, C] {}

object SamplingDistBind {

  def dist: SamplingDistBind[SamplingDist, Dist, SamplingDist, SamplingDistConf] = ???

  def samplingDist: SamplingDistBind[SamplingDist, SamplingDist, SamplingDist, SamplingDistConf] = ???

  def sketch: SamplingDistBind[SamplingDist, Sketch, Sketch, SketchConf] = ???

}
