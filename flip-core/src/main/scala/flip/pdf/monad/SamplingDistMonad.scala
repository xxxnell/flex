package flip.pdf.monad

import flip.conf.{SamplingDistConf, SamplingDistConfB}
import flip.pdf.{Dist, SamplingDist}

import scala.language.higherKinds

trait SamplingDistMonad[
    D1[_] <: SamplingDist[_], D2[_] <: Dist[_], D3[_] <: SamplingDist[_], C <: SamplingDistConfB[D3[_]]]
    extends DistMonad[D1, D2, D3, C]

object SamplingDistMonad {

  def apply(): SamplingDistMonad[SamplingDist, Dist, SamplingDist, SamplingDistConf] = ???

}
