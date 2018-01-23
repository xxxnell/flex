package flip.pdf.monad

import flip.conf.SamplingDistConf
import flip.pdf.{Dist, SamplingDist}

import scala.language.higherKinds

trait SamplingDistMonad[SD1[_]<:SamplingDist[_], D[_]<:Dist[_], SD2[_]<:SamplingDist[_]]
  extends DistMonad[SD1, D, SD2]

object SamplingDistMonad {

  def apply(): SamplingDistMonad[SamplingDist, Dist, SamplingDist] = ???

}