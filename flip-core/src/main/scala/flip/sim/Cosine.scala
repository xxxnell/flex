package flip.sim

import flip.conf.{DistConf, SamplingDistConf}
import flip.pdf.{Dist, PlottedDist, SamplingDist}
import flip.plot.DensityPlot

/**
  * Cosine similarity between two probability density function.
  * https://en.wikipedia.org/wiki/Hilbert_space
  * */
trait Cosine extends DensitySim {

  def norm1: Double

  def norm2: Double

  def point(value1: Double, value2: Double): Double = (value1 * value2) / (norm1 * norm2)

}

object Cosine {

  private case class CosineImpl(norm1: Double, norm2: Double) extends Cosine

  def apply[A](d1: SamplingDist[A],
               d2: Dist[A]): Cosine = {
    val norm1 = normForSamplingDist(d1)
    val norm2 = d2.sampling(d1)
      .map(plottedD2 => normForPlotted(plottedD2))
      .getOrElse(Double.PositiveInfinity)

    CosineImpl(norm1, norm2)
  }

  def apply[A](d1: PlottedDist[A], d2: Dist[A]): Cosine = {
    val norm1 = normForPlotted(d1)
    val norm2 = d2.sampling(d1).map(samplingD2 => normForPlotted(samplingD2))
      .getOrElse(Double.PositiveInfinity)

    CosineImpl(norm1, norm2)
  }

  def normForPlotted[A](d1: PlottedDist[A]): Double = norm(d1.sampling)

  def normForSamplingDist[A](d1: SamplingDist[A]): Double = (for {
    sampling <- d1.sampling
  } yield norm(sampling))
    .getOrElse(Double.PositiveInfinity)

  def norm(pdf: DensityPlot): Double = (for {
    sqr <- Some(pdf.modify { case (_, value) => value * value })
    domain <- sqr.domain
    normsqr = sqr.integral(domain.start, domain.end)
  } yield math.sqrt(normsqr))
    .getOrElse(Double.PositiveInfinity)

}
