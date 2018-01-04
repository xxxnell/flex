package sketch.scope.pdf.update

import sketch.scope.cmap.Cmap
import sketch.scope.pdf.{Count, Prim, Sketch}
import sketch.scope.plot._
import sketch.scope.plot.syntax._
import sketch.scope.range.RangeP

trait EqualSpaceCdfUpdate {

  def updateCmap(sketch: Sketch[_], ps: List[(Prim, Count)],
                 cmapSize: Int, mixingRatio: Double, window: Double): Option[Cmap] = for {
    sketchPlot <- sketch.densityPlot
    mtpSketchPlot = sketchPlot * (1 / (mixingRatio + 1))
    mtpPsPlot = DensityPlot.squareKernel(ps, window) * (mixingRatio / (mixingRatio + 1))
    mergedPlot = if(ps.nonEmpty) mtpSketchPlot + mtpPsPlot else sketchPlot
    cmap = cmapForEqualSpaceCumulative(mergedPlot, cmapSize)
  } yield cmap

  def cmapForEqualSpaceCumulative(plot: DensityPlot, cmapSize: Int): Cmap = {
    val cdf = plot.cumulative
    val invCdf = cdf.inverse
    val unit = cdf.interpolation(Double.MaxValue) / cmapSize.toDouble

    val divider = (1 until cmapSize).toList.map(i => i * unit).map(a => invCdf.interpolation(a))
    Cmap.divider(divider)
  }

  def smoothingPsForEqualSpaceCumulative(ps: List[(Prim, Count)]): CountPlot = {
    val sorted = ps.sortBy(_._1)
    val sliding: List[List[(Prim, Count)]] = sorted.sliding(2).toList
    val headAppendingO: Option[(Prim, Count)] = sliding.headOption.flatMap {
      case d1 :: d2 :: Nil => Some((d1._1 - (d2._1 - d1._1), 0d))
      case _ => None
    }
    val lastAppendingO: Option[(Prim, Count)] = sliding.lastOption.flatMap {
      case d1 :: d2 :: Nil => Some((d2._1 + (d2._1 - d1._1), 0d))
      case _ => None
    }

    val records = (headAppendingO.toList ::: sorted ::: lastAppendingO.toList).sliding(2).toList
      .flatMap {
        case d1 :: d2 :: Nil => Some((RangeP(d1._1, d2._1), (d1._2 + d2._2) / 2))
        case _ => None
      }

    CountPlot.disjoint(records)
  }

}

object EqualSpaceCdfUpdate extends EqualSpaceCdfUpdate