package flex.pdf

import flex.conf.pdf.SamplingDistConf
import flex.plot.PointPlot
import flex.rand.IRng

import scala.language.higherKinds

/**
 * SamplingDist, or Sampling distribution provides the sampling points and its
 * probability density.
 * */
trait SamplingDist[A] extends Dist[A] {

  def conf: SamplingDistConf

}

trait SamplingDistPropOps[D[_] <: SamplingDist[_]] extends DistPropOps[D] with SamplingDistPropLaws[D] {}

trait SamplingDistPropLaws[D[_] <: SamplingDist[_]] { self: SamplingDistPropOps[D] =>

}

object SamplingDist extends SamplingDistPropOps[SamplingDist] {

  def modifyRng[A](dist: SamplingDist[A], f: IRng => IRng): SamplingDist[A] = dist match {
    case dataBinning: DataBinningDist[A] => DataBinningDist.modifyRng(dataBinning, f)
    case (plotted: PlottedDist[A])       => PlottedDist.modifyRng(plotted, f)
    case _                               => ???
  }

  def probability[A](dist: SamplingDist[A], start: A, end: A): Double = dist match {
    case dataBinning: DataBinningDist[A] => DataBinningDist.probability(dataBinning, start, end)
    case plotted: PlottedDist[A]         => PlottedDist.probability(plotted, start, end)
    case _                               => ???
  }

  def cdfSampling[A](dist: SamplingDist[A]): PointPlot = dist match {
    case dataBinning: DataBinningDist[A] => DataBinningDist.cdfSampling(dataBinning)
    case plotted: PlottedDist[A]         => PlottedDist.cdfSampling(plotted)
    case _                               => ???
  }

  override def pdfSampling[A](dist: SamplingDist[A]): PointPlot = dist match {
    case dataBinning: DataBinningDist[A] => DataBinningDist.pdfSampling(dataBinning)
    case plotted: PlottedDist[A]         => PlottedDist.pdfSampling(plotted)
    case _                               => ???
  }

  override def pdf[A](dist: SamplingDist[A], a: A): Prim = dist match {
    case dataBinning: DataBinningDist[_] => DataBinningDist.pdf(dataBinning, a)
    case plotted: PlottedDist[_]         => PlottedDist.pdf(plotted, a)
  }

}
