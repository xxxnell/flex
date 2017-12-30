package sketch.scope.pdf.monad

import sketch.scope.conf.SamplingDistConf
import sketch.scope.measure.Measure
import sketch.scope.pdf.arithmetic.Sum
import sketch.scope.pdf.{Dist, SamplingDist}

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSamplingDistBind
  extends SamplingDistBind[SamplingDist, Dist, SamplingDist, SamplingDistConf] {

  def bind[A, B](dist: SamplingDist[A],
                 f: A => Dist[B],
                 measureB: Measure[B],
                 conf: SamplingDistConf): SamplingDist[B] = ???

}
