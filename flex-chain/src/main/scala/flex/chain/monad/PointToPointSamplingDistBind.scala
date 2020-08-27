package flex.chain.monad

import flex.conf.pdf.SamplingDistConf
import flex.measure.Measure
import flex.pdf.{Dist, SamplingDist}

object PointToPointSamplingDistBind extends SamplingDistBind[SamplingDist, Dist, SamplingDist, SamplingDistConf] {

  def bind[A, B](dist: SamplingDist[A], f: A => Dist[B], measure: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
    ???

}
