package flip.sim

import scala.math._

/**
  * Kullback–Leibler divergence
  * https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  */
object KLD extends DensitySim {

  def point(value1: Double, value2: Double): Double = (value1, value2) match {
    case (0, _) => 0
    case (_, 0) => Double.PositiveInfinity
    case (_, _) => value1 * log(value1 / value2)
  }

}
