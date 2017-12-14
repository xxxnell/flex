package sketch.scope.pdf.monad

import sketch.scope.pdf.{Dist, SampledDist}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistMonad[SD1[_]<:SampledDist[_], D[_]<:Dist[_], SD2[_]<:SampledDist[_]] extends DistMonad[SD1, D, SD2]

object SampleDistMonad {

  def apply(): SampleDistMonad[SampledDist, Dist, SampledDist] = ???

}