package sketch.scope.pdf

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait DataBinningDist[A] extends SamplingDist[A]

trait DataBinningDistOps[D[_]<:DataBinningDist[_]] extends SamplingDistPropOps[D] with DataBinningDistLaws[D] {

  def update[A](dist: D[A], as: List[(A, Count)]): Option[D[A]]

}

trait DataBinningDistLaws[D[_]<:DataBinningDist[_]]