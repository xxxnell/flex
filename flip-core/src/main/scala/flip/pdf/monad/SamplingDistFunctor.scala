package flip.pdf.monad

import flip.conf.{DistConfB, SamplingDistConf, SamplingDistConfB}
import flip.pdf.{Dist, SamplingDist}

import scala.language.higherKinds

trait SamplingDistFunctor[D[_] <: SamplingDist[_], C <: SamplingDistConfB[D[_]]] extends DistFunctor[D, C]

object SamplingDistFunctor {

  def apply(): SamplingDistFunctor[SamplingDist, SamplingDistConf] = ???

}
