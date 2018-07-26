package flip.pdf

import flip.conf.pdf.DataBinningDistConf
import flip.plot.PointPlot
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

  def count[A](dist: D[A], start: A, end: A): Count

  def sum(dist: D[_]): Count

//  def clear(sketch: S): S

}

trait DataBinningDistLaws[D[_] <: DataBinningDist[_]]

object DataBinningDist extends DataBinningDistOps[DataBinningDist] {

  def modifyRng[A](dist: DataBinningDist[A], f: IRng => IRng): DataBinningDist[A] = dist match {
    case sketch: Sketch[A] => Sketch.modifyRng(sketch, f)
    case hist: Histogram[A] => Histogram.modifyRng(hist, f)
  }

  def probability[A](dist: DataBinningDist[A], start: A, end: A): Double = dist match {
    case (sketch: Sketch[A]) => Sketch.probability(sketch, start, end)
    case hist: Histogram[A] => Histogram.probability(hist, start, end)
  }

  def update[A](dist: DataBinningDist[A], as: List[(A, Count)]): DataBinningDist[A] = dist match {
    case sketch: Sketch[A] => Sketch.update(sketch, as)
    case hist: Histogram[A] => Histogram.update(hist, as)
  }

  def count[A](dist: DataBinningDist[A], start: A, end: A): Count = dist match {
    case sketch: Sketch[A] => Sketch.count(sketch, start, end)
    case hist: Histogram[A] => Histogram.count(hist, start, end)
  }

  def sum(dist: DataBinningDist[_]): Count = dist match {
    case sketch: Sketch[_] => Sketch.sum(sketch)
    case hist: Histogram[_] => Histogram.sum(hist)
  }

  def cdfSampling[A](dist: DataBinningDist[A]): PointPlot = dist match {
    case (sketch: Sketch[A]) => Sketch.cdfSampling(sketch)
    case hist: Histogram[A] => Histogram.cdfSampling(hist)
  }

  // overrides

  override def pdf[A](dist: DataBinningDist[A], a: A): Double = dist match {
    case sketch: Sketch[A] => Sketch.pdf(sketch, a)
    case hist: Histogram[A] => Histogram.pdf(hist, a)
  }

}
