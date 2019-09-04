package flex.pdf.monad

import flex.conf.pdf.SamplingDistConf
import flex.measure.Measure
import flex.pdf.{ Dist, PlottedDist, SamplingDist }

object PointToPointDistBind extends DistBind[Dist, Dist, SamplingDist, SamplingDistConf] {

  def bind[A, B](dist: Dist[A], f: A => Dist[B], measure: Measure[B], conf: SamplingDistConf): SamplingDist[B] = {
    val binded = PointToPointBind.bind(dist, f, measure, conf)
    PlottedDist.forPdfSampling(binded.pdfSampling)(measure, conf)
  }

}
