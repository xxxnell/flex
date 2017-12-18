package sketch.scope.pdf.update

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.pdf.{Count, Prim, Sketch}
import sketch.scope.plot._

/**
  * Licensed by Probe Technology, Inc.
  */
trait EqualSpaceCdfUpdate {

  def updateCmap(sketch: Sketch[_], ps: List[(Prim, Count)], mixingRate: Double, window: Double): Option[Cmap] = for {
    sketchPlot <- sketch.densityPlot
    mtpSketchPlot = sketchPlot * (1 / (mixingRate + 1))
    mtpPsPlot = DensityPlot.squareKernel(ps, window) * (mixingRate / (mixingRate + 1))
    mergedPlot = mtpSketchPlot + mtpPsPlot
    caSize <- sketch.structures.headOption.map { case (cmap, _) => cmap.size }
    cmap = cmapForUniformSplitCumulative(mergedPlot, caSize)
  } yield cmap

  def cmapForUniformSplitCumulative(plot: DensityPlot, caSize: Int): Cmap = {
    val cdf = plot.cumulative
    val invCdf = cdf.inverse
    val unit = cdf.interpolation(Double.MaxValue) / caSize.toDouble

    val divider = (1 until caSize).toList.map(i => i * unit).map(a => invCdf.interpolation(a))
    Cmap.divider(divider)
  }

}

object EqualSpaceCdfUpdate extends EqualSpaceCdfUpdate