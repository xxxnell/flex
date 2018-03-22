package flip.pdf.update

import flip.cmap.Cmap
import flip.measure.Measure
import flip.pdf._
import flip.pdf.syntax._
import flip.pdf.sampling.IcdfSampling
import flip.plot._
import flip.plot.syntax._
import flip.range.RangeM

import scala.language.higherKinds

object EqualSpaceCdfUpdate {

  def updateCmap[A](sampling: DensityPlot,
                    ps: List[(Prim, Count)],
                    mixingRatio: Double,
                    window: Double,
                    icdfSampling: (Double => A) => List[RangeM[A]],
                    measure: Measure[A]): Cmap = {
    val pdf = if (ps.nonEmpty) {
      val c1 = 1 / (mixingRatio + 1)
      val c2 = mixingRatio / (mixingRatio + 1)
      (c1, sampling) + (c2, DensityPlot.squareKernel(ps, window))
    } else sampling
    val icdfPlot = pdf.inverseCumulative
    val icdf = (d: Double) => measure.from(icdfPlot.interpolation(d))
    val ranges = icdfSampling(icdf)
    val divider = ranges
      .flatMap(rangeM => rangeM.primitivize.start :: rangeM.primitivize.end :: Nil)
      .distinct
      .filter(d => !d.isInfinity)

    Cmap.divider(divider)
  }

  def updateCmapForSketch[A](sketch: Sketch[A], ps: List[(Prim, Count)]): Cmap = {
    val sampling = sketch.sampling
    val mixingRatio = sketch.conf.mixingRatio
    val window = sketch.conf.dataKernelWindow
    val measure = sketch.measure

    updateCmap(sampling, ps, mixingRatio, window, IcdfSampling.samplingF(measure, sketch.conf.cmap), measure)
  }

  /**
    * @param corr boundary marginal ratio for the separation unit.
    *             If corr=1, cmapForEqualSpaceCumCorr is identical to standard cmapForEqualSpaceCum.
    *             If corr=0, cmap has no margin.
    * */
  @deprecated
  def cmapForEqualSpaceCumCorr(plot: DensityPlot, corr: Double, cmapSize: Int): Cmap = {
    lazy val icdf = plot.inverseCumulative

    val cdfDivider = if (cmapSize < 2) {
      Nil
    } else if (cmapSize == 2) {
      0.5 :: Nil
    } else {
      val maxAccumulative = icdf.domain.map(_.end).getOrElse(1.0)
      val unit = maxAccumulative / (cmapSize.toDouble - 2 + 2 * corr)

      (1 until cmapSize).toList
        .map(i => unit * corr + unit * (i - 1))
    }

    val pDivider = cdfDivider.map(a => icdf.interpolation(a))

    Cmap.divider(pDivider)
  }

}
