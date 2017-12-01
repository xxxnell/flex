package sketch.scope.pdf.monad

import sketch.scope.pdf.{Dist, SampleDist}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistBind[SampleD1[_]<:SampleDist[_], D[_]<:Dist[_], SampleD2[_]<:SampleDist[_]]
  extends DistBind[SampleD1, D, SampleD2] {

  def flatMap[A, B](dist: SampleD1[A], f: A => D[B]): SampleD2[B]

}
