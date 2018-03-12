package flip.pdf

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.plot.DensityPlot
import flip.range.RangeM
import flip.rand.IRng

import scala.language.higherKinds

/**
  * SamplingDist, or Sampling distribution provides the sampling points and its
  * probability density.
  * */
trait SamplingDist[A] extends Dist[A] {

  def conf: SamplingDistConf

}

trait SamplingDistPropOps[D[_] <: SamplingDist[_]] extends DistPropOps[D] with SamplingDistPropLaws[D] {

  def sampling[A](dist: D[A]): DensityPlot

}

trait SamplingDistPropLaws[D[_] <: SamplingDist[_]] { self: SamplingDistPropOps[D] =>

  def cdfPlot[A](dist: D[A]): DensityPlot = {
    sampling(dist).cumulative
  }

  def icdfPlot[A](dist: D[A]): DensityPlot = {
    cdfPlot(dist).inverse
  }

  def pdf[A](dist: D[A], a: A): Double = {
    val plot = sampling(dist)
    DensityPlot.interpolation(plot, dist.measure.asInstanceOf[Measure[A]].to(a))
  }

  def cdf[A](dist: D[A], a: A): Double = {
    val cdf = cdfPlot(dist)
    val p = dist.measure.asInstanceOf[Measure[A]].to(a)
    cdf.interpolation(p)
  }

  def icdf[A](dist: D[A], p: Double): A = {
    val icdf = icdfPlot(dist)
    val measure = dist.measure.asInstanceOf[Measure[A]]
    measure.from(icdf.interpolation(p))
  }

}

object SamplingDist extends SamplingDistPropOps[SamplingDist] {

  def forSmoothDist[A](dist: SmoothDist[A], domains: List[RangeM[A]]): SamplingDist[A] =
    SmoothDist.samplingDist(dist, domains)

  def modifyRng[A](dist: SamplingDist[A], f: IRng => IRng): SamplingDist[A] = dist match {
    case dataBinning: DataBinningDist[A] => DataBinningDist.modifyRng(dataBinning, f)
    case (plotted: PlottedDist[A]) => PlottedDist.modifyRng(plotted, f)
    case _ => ???
  }

  def probability[A](dist: SamplingDist[A], start: A, end: A): Double = dist match {
    case dataBinning: DataBinningDist[A] => DataBinningDist.probability(dataBinning, start, end)
    case plotted: PlottedDist[A] => PlottedDist.probability(plotted, start, end)
    case _ => ???
  }

  def sampling[A](dist: SamplingDist[A]): DensityPlot = dist match {
    case dataBinning: DataBinningDist[A] => DataBinningDist.sampling(dataBinning)
    case plotted: PlottedDist[A] => PlottedDist.sampling(plotted)
    case _ => ???
  }

  override def pdf[A](dist: SamplingDist[A], a: A): Prim = dist match {
    case dataBinning: DataBinningDist[_] => DataBinningDist.pdf(dataBinning, a)
    case plotted: PlottedDist[_] => PlottedDist.pdf(plotted, a)
  }

}
