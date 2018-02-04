package flip.pdf.update

import flip.cmap.Cmap
import flip.pdf._
import flip.plot._
import flip.plot.syntax._
import flip.range.RangeP

trait EqualSpaceCdfUpdate {

  def updateCmapForSketch(sketch: Sketch[_], ps: List[(Prim, Count)]): Option[Cmap] = for {
    sketchSamples <- flip.time(sketch.sampling, "sampling", false) // 3e7
    mixingRatio = sketch.conf.mixingRatio
    window = sketch.conf.dataKernelWindow
    corr = sketch.conf.boundaryCorrection
    cmapSize = sketch.conf.cmap.size
  } yield updateCmap(sketchSamples, ps, mixingRatio, window, corr, cmapSize)

  def updateCmap(sketchSamples: DensityPlot, ps: List[(Prim, Count)],
                 mixingRatio: Double, window: Double, corr: Double, cmapSize: Int): Cmap = {
    val mergedPlot = flip.time(if(ps.nonEmpty) {
      val c1 = 1 / (mixingRatio + 1)
      val c2 = mixingRatio / (mixingRatio + 1)
      (c1, sketchSamples) + (c2, DensityPlot.squareKernel(ps, window))
    } else sketchSamples, "mergedPlot", false) // 2e7 vs 2e4

    flip.time(cmapForEqualSpaceCumCorr(mergedPlot, corr, cmapSize), "cmapForEqualSpaceCumCorr", false) // 7e7 vs 2e7
  }

  /**
    * @param corr boundary marginal ratio for the separation unit.
    *             If corr=1, cmapForEqualSpaceCumCorr is identical to standard cmapForEqualSpaceCum.
    *             If corr=0, cmap has no margin.
    * */
  def cmapForEqualSpaceCumCorr(plot: DensityPlot, corr: Double, cmapSize: Int): Cmap = {
    lazy val invCdf = flip.time(plot.inverseCumulative, "invCdf", false) // 2e6

    val cdfDivider = if(cmapSize < 2) {
      Nil
    } else if(cmapSize == 2) {
      0.5 :: Nil
    } else {
      val maxAccumulative = invCdf.domain.map(_.end).getOrElse(1.0)
      val unit = maxAccumulative / (cmapSize.toDouble - 2 + 2 * corr)

      flip.time((1 until cmapSize).toList
        .map(i => unit * corr + unit * (i - 1)), "cdfDivider", false) // 2e5
    }

    val pDivider = flip.time(cdfDivider.map(a => invCdf.interpolation(a)), "pDivider", false) // 3e6

    flip.time(Cmap.divider(pDivider), "Cmap.divider", false) // 3e6
  }

}

object EqualSpaceCdfUpdate extends EqualSpaceCdfUpdate