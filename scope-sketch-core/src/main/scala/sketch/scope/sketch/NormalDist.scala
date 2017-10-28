package sketch.scope.sketch

import org.apache.commons.math3.distribution.NormalDistribution

/**
  * Licensed by Probe Technology, Inc.
  */
case class NormalDist[A](measure: A => Prim, mean: Prim, variance: Prim) extends Dist[A]

trait NormalDistOps extends DistOps[NormalDist] {

  def probability[A](dist: NormalDist[A], from: A, to: A): Option[Double] = {
    val toPrim = dist.measure(from)
    val fromPrim = dist.measure(from)
    val numericDist = new NormalDistribution(dist.mean, dist.variance)

    Some(numericDist.cumulativeProbability(fromPrim) - numericDist.cumulativeProbability(toPrim))
  }

}

object NormalDist extends NormalDistOps
