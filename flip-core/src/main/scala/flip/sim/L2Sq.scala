package flip.sim

import flip.conf.{DistConf, SamplingDistConf}
import flip.pdf.{Dist, PlottedDist, SamplingDist}
import flip.plot.DensityPlot

/**
  * Squared Eucledian Distance, or Minkowski L2 Distance between two pdfs.
  * https://en.wikipedia.org/wiki/Euclidean_distance#Squared_Euclidean_distance
  * https://en.wikipedia.org/wiki/Hilbert_space
  * */
object L2Sq extends DensitySim {

  def point(value1: Double, value2: Double): Double = {
    val diff = value1 - value2
    diff * diff
  }

}
