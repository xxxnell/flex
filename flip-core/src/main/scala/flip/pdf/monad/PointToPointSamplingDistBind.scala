package flip.pdf.monad

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.pdf.arithmetic.Sum
import flip.pdf.{Dist, SamplingDist}

object PointToPointSamplingDistBind extends SamplingDistBind[SamplingDist, Dist, SamplingDist] {

  def bind[A, B](dist: SamplingDist[A], f: A => Dist[B], measureB: Measure[B]): SamplingDist[B] = ???

}
