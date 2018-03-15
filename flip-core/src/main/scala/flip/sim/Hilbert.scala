package flip.sim

import flip.pdf.Dist
import flip.plot.DensityPlot

/**
  * Set of operations for distribution on Hilbert space.
  * */
object Hilbert {

  def normForSamplingDist[A](d1: Dist[A]): Double = normForPlot(d1.sampling)

  def normForPlot(pdf: DensityPlot): Double = {
    val sqr = pdf.modify { case (_, value) => value * value }
    val normsqr = sqr.integralAll
    math.sqrt(normsqr)
  }

}
