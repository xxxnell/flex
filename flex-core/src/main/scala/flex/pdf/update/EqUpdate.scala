package flex.pdf.update

import flex.cmap.Cmap
import flex.measure.Measure
import flex.pdf._
import flex.pdf.syntax._
import flex.pdf.sampling.IcdfSampling
import flex.plot._
import flex.plot.syntax._
import flex.range.RangeM
import flex.measure.syntax._

import scala.language.higherKinds

object EqUpdate {

  def updateCmap[A](
      cdfSampling: PointPlot,
      ps: List[(Prim, Count)],
      mixingRatio: Double,
      window: Double,
      icdfSampling: (Double => A) => List[A],
      measure: Measure[A]): Cmap = {
    val mergedCum =
      if (ps.isEmpty) cdfSampling
      else if (cdfSampling.isEmpty) PointPlot.normalizedCumulative(ps)
      else {
        val c1 = 1 / (mixingRatio + 1)
        val c2 = mixingRatio / (mixingRatio + 1)
        (c1, cdfSampling) :+ (c2, PointPlot.normalizedCumulative(ps))
      }
    val icdfPlot = mergedCum.inverse
    def icdf: Double => A =
      (d: Double) =>
        if (d <= 0) measure.from(-∞) else if (d >= 1) measure.from(∞) else measure.from(icdfPlot.interpolation(d))
    val divider = icdfSampling(icdf).map(a => measure.to(a))

    Cmap.divider(divider)
  }

  def updateCmapForSketch[A](sketch: Sketch[A], ps: List[(Prim, Count)]): Cmap = {
    val cdfSampling = sketch.cdfSampling
    val mixingRatio = sketch.conf.mixingRatio
    val window = sketch.conf.dataKernelWindow
    val measure = sketch.measure

    updateCmap(cdfSampling, ps, mixingRatio, window, IcdfSampling.samplingF(measure, sketch.conf.cmap), measure)
  }

}
