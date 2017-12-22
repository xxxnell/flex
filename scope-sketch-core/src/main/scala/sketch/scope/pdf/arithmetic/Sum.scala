package sketch.scope.pdf.arithmetic

import sketch.scope.measure.Measure
import sketch.scope.pdf.{Dist, PredefinedDist}

/**
  * Licensed by Probe Technology, Inc.
  */
object Sum {

  def weightedSum[A](weightDists: List[(Double, Dist[A])], measureB: Measure[A]): PredefinedDist[A] = {
    // todo normalize the weights
    PredefinedDist[A](measureB, (from: A, to: A) => probabilityForWeightDists(from, to, weightDists))
  }

  def probabilityForWeightDists[A](from: A, to: A, weightDists: List[(Double, Dist[A])]): Option[Double] =
    weightDists.foldLeft(Option(0d)){ case (accProbO, (weight, distB)) =>
      for {
        accProb <- accProbO
        probB <- distB.probability(from, to)
      } yield accProb + weight * probB
    }

}
