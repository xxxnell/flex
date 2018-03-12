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

  def modifyRng[A](dist: DataBinningDist[A], f: IRng => IRng): DataBinningDist[A] = dist match {
    case sketch: Sketch[A] => Sketch.modifyRng(sketch, f)
  }

  def probability[A](dist: DataBinningDist[A], start: A, end: A): Double = dist match {
    case (sketch: Sketch[A]) => Sketch.probability(sketch, start, end)
    case _ => ???
  }

  def sampling[A](dist: DataBinningDist[A]): DensityPlot = dist match {
    case (sketch: Sketch[A]) => Sketch.sampling(sketch)
    case _ => ???
  }

  def update[A](dist: DataBinningDist[A], as: List[(A, Count)]): DataBinningDist[A] = dist match {
    case sketch: Sketch[A] => Sketch.update(sketch, as)
  }

  override def pdf[A](dist: DataBinningDist[A], a: A): Double = dist match {
    case sketch: Sketch[A] => Sketch.pdf(sketch, a)
  }

}
