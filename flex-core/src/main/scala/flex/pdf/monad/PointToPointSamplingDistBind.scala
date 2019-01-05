package flex.pdf.monad

import flex.conf.pdf.SamplingDistConf
import flex.measure.Measure
import flex.pdf.{Dist, SamplingDist}

object PointToPointSamplingDistBind extends SamplingDistBind[SamplingDist, Dist, SamplingDist, SamplingDistConf] {

  def bind[A, B](dist: SamplingDist[A], f: A => Dist[B], measure: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
//    val binded = PointToPointBind.bind(dist, f, measure, conf)
//    PlottedDist.densityPlot(binded.sampling)(measure, conf)
    ???

}
