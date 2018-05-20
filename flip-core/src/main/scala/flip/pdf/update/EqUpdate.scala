package flip.pdf.update

import flip.cmap.Cmap
import flip.measure.Measure
import flip.pdf._
import flip.pdf.syntax._
import flip.pdf.sampling.IcdfSampling
import flip.plot._
import flip.plot.syntax._
import flip.range.RangeM
import flip.measure.syntax._

import scala.language.higherKinds

object EqUpdate {

  def updateCmap[A](sampling: PointPlot,
                    ps: List[(Prim, Count)],
                    mixingRatio: Double,
                    window: Double,
                    icdfSampling: (Double => A) => List[A],
                    measure: Measure[A]): Cmap = {
    val mergedCum = if (ps.nonEmpty) {
      val c1 = 1 / (mixingRatio + 1)
      val c2 = mixingRatio / (mixingRatio + 1)
      (c1, sampling.normalizedCumulative) :+ (c2, PointPlot.normalizedCumulative(ps))
    } else sampling
    val icdfPlot = mergedCum.inverse
    def icdf =
      (d: Double) =>
        if (d <= 0) measure.from(-∞) else if (d >= 1) measure.from(∞) else measure.from(icdfPlot.interpolation(d))
    val divider = icdfSampling(icdf).map(a => measure.to(a))

    Cmap.divider(divider)
  }

  def updateCmapForSketch[A](sketch: Sketch[A], ps: List[(Prim, Count)]): Cmap = {
    val sampling = sketch.pointSampling
    val mixingRatio = sketch.conf.mixingRatio
    val window = sketch.conf.dataKernelWindow
    val measure = sketch.measure

    updateCmap(sampling, ps, mixingRatio, window, IcdfSampling.samplingF(measure, sketch.conf.cmap), measure)
  }

}
