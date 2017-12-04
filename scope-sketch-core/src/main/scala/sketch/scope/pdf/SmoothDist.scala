package sketch.scope.pdf

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDist[A] extends Dist[A]

trait SmoothDistPropOps[D[_]<:SmoothDist[_]] extends DistPropOps[D]

object SmoothDist extends SmoothDistPropOps[SmoothDist] {

  def probability[A](dist: SmoothDist[A], from: A, to: A): Option[Prim] = dist match {
    case dist: DeltaDist[A] => DeltaDist.probability(dist, from, to)
  }

}