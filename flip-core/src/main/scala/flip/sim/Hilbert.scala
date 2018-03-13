package flip.sim

import flip.pdf.Dist
import flip.plot.DensityPlot

/**
  * Set of operations for distribution on Hilbert space.
  * */
object Hilbert {

  def normForSamplingDist[A](d1: Dist[A]): Double = normForPlot(d1.sampling)

  def normForPlot(pdf: DensityPlot): Double =
    (for {
      sqr <- Some(pdf.modify { case (_, value) => value * value })
      domain <- sqr.domain
      normsqr = sqr.integral(domain.start, domain.end)
    } yield math.sqrt(normsqr))
      .getOrElse(Double.PositiveInfinity)

}
