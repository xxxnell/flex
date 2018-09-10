package flip.chain.monad

import flip.conf.pdf.{SamplingDistConf, SamplingDistConfB}
import flip.pdf.SamplingDist

import scala.language.higherKinds

trait SamplingDistFunctor[D1[_] <: SamplingDist[_], D2[_] <: SamplingDist[_], C <: SamplingDistConfB[D2[_]]]
    extends DistFunctor[D1, D2, C]

object SamplingDistFunctor {

  def default: SamplingDistFunctor[SamplingDist, SamplingDist, SamplingDistConf] = CombinatorialSamplingDistFunctor

}
