package flip.sim

import flip.pdf.{PlottedDist, SamplingDist}
import flip.plot.DensityPlot

/**
  * Set of operations for distribution on Hilbert space.
  * */
object Hilbert {

  def normForSamplingDist[A](d1: SamplingDist[A]): Double = (for {
    sampling <- d1.sampling
  } yield normForPlot(sampling))
    .getOrElse(Double.PositiveInfinity)

  def normForPlot(pdf: DensityPlot): Double = (for {
    sqr <- Some(pdf.modify { case (_, value) => value * value })
    domain <- sqr.domain
    normsqr = sqr.integral(domain.start, domain.end)
  } yield math.sqrt(normsqr))
    .getOrElse(Double.PositiveInfinity)


}
