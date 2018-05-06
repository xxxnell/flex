package flip.sim

import flip.plot.PointPlot

import scala.math._

/**
  * Kullback–Leibler divergence.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Kullback-Leibler_divergence">
  *        Kullback–Leibler divergence - Wikipedia</a>
  */
object KLD extends DensitySim {

  val cutoff = 1E-300

  def point(value1: Double, value2: Double): Double = {
    if (Math.abs(value1) < cutoff) 0
    else if (Math.abs(value2) < cutoff) Double.PositiveInfinity
    else value1 * log(value1 / value2)
  }

  override def simDensity[A](sampling1: PointPlot, sampling2: PointPlot): PointPlot = {
    PointPlot(super.simDensity(sampling1, sampling2).records.filter { case (_, y) => !y.isPosInfinity })
  }

}
