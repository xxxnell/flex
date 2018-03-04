package flip.pdf.arithmetic

import flip.conf.{DefaultSketchConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.{Dist, PredefinedDist}

object Sum {

  def weightedSum[A](weightDists: List[(Double, Dist[A])], measureB: Measure[A]): PredefinedDist[A] = {
    val conf = weightDists
      .map { case (_, dist) => dist.conf }
      .headOption
      .map(distConf => SmoothDistConf.forDistConf(distConf))
      .getOrElse(SmoothDistConf())
    PredefinedDist.bare[A](measureB, conf, (from: A, to: A) => probabilityForWeightDists(from, to, weightDists))
  }

  def probabilityForWeightDists[A](from: A, to: A, weightDists: List[(Double, Dist[A])]): Double =
    // todo normalize the weights
    weightDists.foldLeft(0.0) { case (accProb, (weight, distB)) => accProb + weight * distB.probability(from, to) }

}
