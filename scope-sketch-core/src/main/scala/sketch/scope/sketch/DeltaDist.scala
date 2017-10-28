package sketch.scope.sketch

import sketch.scope.range._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Dirac Delta Function.
  */
case class DeltaDist[A](measure: A => Prim) extends Dist[A]

trait DeltaDistOps extends DistOps[DeltaDist] {

  def probability[A](dist: DeltaDist[A], from: A, to: A): Option[Double] = {
    if(Range(dist.measure(from), dist.measure(to)).contains(0)) Some(1) else Some(0)
  }

}

object DeltaDist extends DeltaDistOps