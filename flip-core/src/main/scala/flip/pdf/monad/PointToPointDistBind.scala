package flip.pdf.monad

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.pdf.{Dist, PlottedDist, SamplingDist}

object PointToPointDistBind extends DistBind[Dist, Dist, SamplingDist, SamplingDistConf] {

  def bind[A, B](dist: Dist[A], f: A => Dist[B], measure: Measure[B], conf: SamplingDistConf): SamplingDist[B] = {
    val binded = PointToPointBind.bind(dist, f, measure, conf)
    PlottedDist.pointPlot(binded.sampling)(measure, conf)
  }

}
