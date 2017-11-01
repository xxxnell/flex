package sketch.scope.sketch.algebra

import sketch.scope.sketch.{Dist, SampleDist}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistMonad[SD1[_]<:SampleDist[_], D[_]<:Dist[_], SD2[_]<:SampleDist[_]] extends DistMonad[SD1, D, SD2]

object SampleDistMonad {

  def apply(): SampleDistMonad[SampleDist, Dist, SampleDist] = ???

}