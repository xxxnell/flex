package flip.pdf

import flip.conf.DataBinningDistConf

import scala.language.higherKinds

/**
  * Data binning distribution is Sampling distribution, and it ensures that you
  * can update the dataset.
  * */
trait DataBinningDist[A] extends SamplingDist[A]

trait DataBinningDistOps[D[_]<:DataBinningDist[_]]
  extends SamplingDistPropOps[D]
    with DataBinningDistLaws[D] {

  def update[A](dist: D[A], as: List[(A, Count)]): Option[D[A]]

}

trait DataBinningDistLaws[D[_]<:DataBinningDist[_]]