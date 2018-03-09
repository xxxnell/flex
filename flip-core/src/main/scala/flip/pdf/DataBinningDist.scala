package flip.pdf

import flip.conf.DataBinningDistConf
import flip.plot.DensityPlot
import flip.rand.IRng

import scala.language.higherKinds

/**
  * Data binning distribution is Sampling distribution, and it ensures that you
  * can update the dataset.
  * */
trait DataBinningDist[A] extends SamplingDist[A] {

  def conf: DataBinningDistConf

}

trait DataBinningDistOps[D[_] <: DataBinningDist[_]] extends SamplingDistPropOps[D] with DataBinningDistLaws[D] {

  def update[A](dist: D[A], as: List[(A, Count)]): D[A]

}

trait DataBinningDistLaws[D[_] <: DataBinningDist[_]]

object DataBinningDist extends DataBinningDistOps[DataBinningDist] {

  def probability[A](dist: DataBinningDist[A], start: A, end: A): Double = dist match {
    case (sketch: Sketch[A]) => Sketch.probability(sketch, start, end)
    case _ => ???
  }

  def sampling[A](dist: DataBinningDist[A]): DensityPlot = dist match {
    case (sketch: Sketch[A]) => Sketch.sampling(sketch)
    case _ => ???
  }

  def sample[A](dist: DataBinningDist[A]): (DataBinningDist[A], A) = dist match {
    case sketch: Sketch[_] => Sketch.sample(sketch)
  }

  def update[A](dist: DataBinningDist[A], as: List[(A, Count)]): DataBinningDist[A] = dist match {
    case sketch: Sketch[A] => Sketch.update(sketch, as)
  }

  def modifyRng[A](dist: DataBinningDist[A], f: IRng => IRng): DataBinningDist[A] = dist match {
    case sketch: Sketch[A] => Sketch.modifyRng(sketch, f)
  }

}
