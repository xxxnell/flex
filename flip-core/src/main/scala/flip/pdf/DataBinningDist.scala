package flip.pdf

import flip.conf.DataBinningDistConf

import scala.language.higherKinds

/**
  * Data binning distribution.
  * */
trait DataBinningDist[A] extends SamplingDist[A]

trait DataBinningDistOps[D[_]<:DataBinningDist[_], C<:DataBinningDistConf]
  extends SamplingDistPropOps[D, C]
    with DataBinningDistLaws[D] {

  def update[A](dist: D[A], as: List[(A, Count)], conf: C): Option[D[A]]

}

trait DataBinningDistLaws[D[_]<:DataBinningDist[_]]