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
    cmapSize = sketch.conf.cmap.size
//    _ = println(s"mixingRate: $mixingRate, window: $window")
//    _ = println(s"mergedPlot: $mergedPlot")
    cmap = cmapForEqualSpaceCumulative(mergedPlot, cmapSize)
  } yield cmap

  def cmapForEqualSpaceCumulative(plot: DensityPlot, cmapSize: Int): Cmap = {
    val cdf = plot.cumulative
    val invCdf = cdf.inverse
    val unit = cdf.interpolation(Double.MaxValue) / cmapSize.toDouble

//    println(s"cdf: $cdf")
//    println(s"invCdf: $invCdf")
//    println(s"cdf.interpolation(Double.MaxValue): ${cdf.interpolation(Double.MaxValue)} unit: " + unit)

    val divider = (1 until cmapSize).toList.map(i => i * unit).map(a => {
//      println("interpolation: " + invCdf.interpolation(a) + s" for a: $a")
      invCdf.interpolation(a)
    })
    Cmap.divider(divider)
  }

}

object EqualSpaceCdfUpdate extends EqualSpaceCdfUpdate