package sketch.scope.pdf

import org.apache.commons.math3.distribution.NormalDistribution
import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
case class NormalDist[A](measure: Measure[A], mean: Prim, variance: Prim) extends SmoothDist[A]

trait NormalDistOps extends SmoothDistPropOps[NormalDist] {

  def probability[A](dist: NormalDist[A], from: A, to: A): Option[Double] = {
    val toPrim = dist.measure(to)
    val fromPrim = dist.measure(from)
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    Some(numericDist.cumulativeProbability(toPrim) - numericDist.cumulativeProbability(fromPrim))
  }

}

object NormalDist extends NormalDistOps
