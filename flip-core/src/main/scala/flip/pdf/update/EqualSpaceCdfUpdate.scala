package flip.pdf.update

import flip.cmap.Cmap
import flip.conf.SketchConf
import flip.pdf._
import flip.plot._
import flip.plot.syntax._
import flip.range.RangeP

trait EqualSpaceCdfUpdate {

  def updateCmap(sketch: Sketch[_], ps: List[(Prim, Count)], conf: SketchConf): Option[Cmap] = for {
    sketchPlot <- sketch.sampling(conf)
    mixingRatio = conf.mixingRatio
    window = conf.dataKernelWindow
    cmapSize = conf.cmap.size
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

  def smoothingPsForEqualSpaceCumulative(ps: List[(Prim, Count)]): DensityPlot = {
    val sorted = ps.sortBy(_._1)
    val sliding: List[List[(Prim, Count)]] = sorted.sliding(2).toList
    val headAppendingO: Option[(Prim, Count)] = sliding.headOption.flatMap {
      case (p1, _) :: (p2, _) :: Nil => Some((p1 - (p2 - p1), 0d))
      case _ => None
    }
    val lastAppendingO: Option[(Prim, Count)] = sliding.lastOption.flatMap {
      case (p1, _) :: (p2, _) :: Nil => Some((p2 + (p2 - p1), 0d))
      case _ => None
    }

    val records = (headAppendingO.toList ::: sorted ::: lastAppendingO.toList)
      .sliding(2).toList
      .flatMap {
        case (p1, count1) :: (p2, count2) :: Nil if !p1.isInfinity && !p2.isInfinity =>
          val range = RangeP(p1, p2)
          if(!range.isPoint) Some((range, (count1 + count2) / (2 * range.length).toDouble)) else None
        case _ => None
      }

    DensityPlot.disjoint(records)
  }

}

object EqualSpaceCdfUpdate extends EqualSpaceCdfUpdate