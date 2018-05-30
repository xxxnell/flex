package flip.pdf

import flip.conf.pdf.SamplingDistConf
import flip.measure.Measure
import flip.plot.PointPlot
import flip.rand.IRng

import scala.language.higherKinds

/**
  * PlottedDist, or Plotted Distribution has a plot with specific quantization
  * points and their probability densities.
  * */
trait PlottedDist[A] extends SamplingDist[A] {

  def cdfSampling: PointPlot

}

trait PlottedDistPropOps[D[_] <: PlottedDist[_]] extends SamplingDistPropOps[D] {

  def modifySampling[A](dist: D[A], f: PointPlot => PointPlot): D[A]

  def cdfSampling[A](dist: D[A]): PointPlot = dist.cdfSampling

  def probability[A](dist: D[A], start: A, end: A): Double = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val cum = cdfSampling(dist)
    cum.interpolation(measure.to(end)) - cum.interpolation(measure.to(start))
  }

}

object PlottedDist extends PlottedDistPropOps[PlottedDist] {

  private case class PlottedDistImpl[A](measure: Measure[A], rng: IRng, cdfSampling: PointPlot, conf: SamplingDistConf)
      extends PlottedDist[A]

  def bare[A](measure: Measure[A], rng: IRng, sampling: PointPlot, conf: SamplingDistConf): PlottedDist[A] =
    PlottedDistImpl(measure, rng, sampling, conf)

  def forPdfSampling[A](pdfSampling: PointPlot)(implicit measure: Measure[A], conf: SamplingDistConf): PlottedDist[A] =
    bare(measure, IRng(pdfSampling.hashCode()), pdfSampling.normalizedCumulative, conf)

  def forCdfSampling[A](cdfSampling: PointPlot)(implicit measure: Measure[A], conf: SamplingDistConf): PlottedDist[A] =
    bare(measure, IRng(cdfSampling.hashCode()), cdfSampling, conf)

  def modifyRng[A](dist: PlottedDist[A], f: IRng => IRng): PlottedDist[A] =
    bare(dist.measure, f(dist.rng), dist.pdfSampling, dist.conf)

  def modifySampling[A](dist: PlottedDist[A], f: PointPlot => PointPlot): PlottedDist[A] =
    bare(dist.measure, dist.rng, f(dist.pdfSampling), dist.conf)

}
