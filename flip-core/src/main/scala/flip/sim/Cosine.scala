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

  def apply[A](d1: SamplingDist[A], conf1: SamplingDistConf,
               d2: Dist[A], conf2: DistConf): Cosine = {
    val norm1 = normForSamplingDist(d1, conf1)
    val norm2 = d2.sampling(d1, conf1)(conf2)
      .map(plottedD2 => normForPlotted(plottedD2))
      .getOrElse(Double.PositiveInfinity)

    CosineImpl(norm1, norm2)
  }

  def apply[A](d1: PlottedDist[A], d2: Dist[A], conf2: DistConf): Cosine = {
    val norm1 = normForPlotted(d1)
    val norm2 = d2.sampling(d1)(conf2).map(samplingD2 => normForPlotted(samplingD2))
      .getOrElse(Double.PositiveInfinity)

    CosineImpl(norm1, norm2)
  }

  def normForPlotted[A](d1: PlottedDist[A]): Double = norm(d1.sampling)

  def normForSamplingDist[A](d1: SamplingDist[A], conf: SamplingDistConf): Double = (for {
    sampling <- d1.sampling(conf)
  } yield norm(sampling))
    .getOrElse(Double.PositiveInfinity)

  def norm(pdf: DensityPlot): Double =
    math.sqrt(pdf.modify { case (_, value) => value * value }.integral(Double.MinValue, Double.MaxValue))

}
