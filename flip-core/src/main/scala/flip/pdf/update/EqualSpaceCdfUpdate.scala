package flip.pdf.update

import flip.cmap.Cmap
import flip.pdf._
import flip.plot._
import flip.plot.syntax._
import flip.range.RangeP

trait EqualSpaceCdfUpdate {

  def updateCmapForSketch(sketch: Sketch[_], ps: List[(Prim, Count)]): Cmap = {
    val sketchSamples = sketch.sampling
    val mixingRatio = sketch.conf.mixingRatio
    val window = sketch.conf.dataKernelWindow
    val corr = sketch.conf.boundaryCorrection
    val cmapSize = sketch.conf.cmap.size

    updateCmap(sketchSamples, ps, mixingRatio, window, corr, cmapSize)
  }

  def updateCmap(sketchSamples: DensityPlot,
                 ps: List[(Prim, Count)],
                 mixingRatio: Double,
                 window: Double,
                 corr: Double,
                 cmapSize: Int): Cmap = {
    val mergedPlot = if (ps.nonEmpty) {
      val c1 = 1 / (mixingRatio + 1)
      val c2 = mixingRatio / (mixingRatio + 1)
      (c1, sketchSamples) + (c2, DensityPlot.squareKernel(ps, window))
    } else sketchSamples

    cmapForEqualSpaceCumCorr(mergedPlot, corr, cmapSize)
  }

  /**
    * @param corr boundary marginal ratio for the separation unit.
    *             If corr=1, cmapForEqualSpaceCumCorr is identical to standard cmapForEqualSpaceCum.
    *             If corr=0, cmap has no margin.
    * */
  def cmapForEqualSpaceCumCorr(plot: DensityPlot, corr: Double, cmapSize: Int): Cmap = {
    lazy val invCdf = plot.inverseCumulative

    val cdfDivider = if (cmapSize < 2) {
      Nil
    } else if (cmapSize == 2) {
      0.5 :: Nil
    } else {
      val maxAccumulative = invCdf.domain.map(_.end).getOrElse(1.0)
      val unit = maxAccumulative / (cmapSize.toDouble - 2 + 2 * corr)

      (1 until cmapSize).toList
        .map(i => unit * corr + unit * (i - 1))
    }

    val pDivider = cdfDivider.map(a => invCdf.interpolation(a))

    Cmap.divider(pDivider)
  }

}

object EqualSpaceCdfUpdate extends EqualSpaceCdfUpdate
