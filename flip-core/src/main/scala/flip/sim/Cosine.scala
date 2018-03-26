package flip.sim

import flip.conf.{DistConf, SamplingDistConf}
import flip.pdf.{Dist, PlottedDist, SamplingDist}
import flip.plot.DensityPlot

/**
  * Cosine similarity between two probability density function.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Cosine_similarity">Cosine similarity - Wikipedia</a>
  * */
trait Cosine extends DensitySim {

  def norm1: Double

  def norm2: Double

  def point(value1: Double, value2: Double): Double = (value1 * value2) / (norm1 * norm2)

}

object Cosine {

  private case class CosineImpl(norm1: Double, norm2: Double) extends Cosine

  def apply[A](d1: Dist[A], d2: Dist[A]): Cosine = {
    val norm1 = Hilbert.normForSamplingDist(d1)
    val norm2 = Hilbert.normForSamplingDist(d2)

    CosineImpl(norm1, norm2)
  }

}
