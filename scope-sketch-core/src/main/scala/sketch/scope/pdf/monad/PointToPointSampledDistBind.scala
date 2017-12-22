package sketch.scope.pdf.monad

import sketch.scope.measure.Measure
import sketch.scope.pdf.arithmetic.Sum
import sketch.scope.pdf.{Dist, SampledDist}

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSampledDistBind extends SampleDistBind[SampledDist, Dist, SampledDist] {

  def bind[A, B](dist: SampledDist[A], f: A => Dist[B], measureB: Measure[B]): SampledDist[B] = ???

}
