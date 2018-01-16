package flip.pdf.update

import flip.time
import flip.cmap.Cmap
import flip.conf.{SamplingDistConf, SketchConf}
import flip.pdf._
import flip.plot._
import flip.plot.syntax._
import flip.range.RangeP

trait EqualSpaceCdfUpdate {

  def updateCmap(sketch: Sketch[_], ps: List[(Prim, Count)], conf: SketchConf): Option[Cmap] = for {
    sketchPlot <- time(sketch.sampling(conf), "sampling", false) // 3E6
    mixingRatio = conf.mixingRatio
    window = conf.dataKernelWindow
    cmapSize = conf.cmap.size
    mtpSketchPlot = sketchPlot * (1 / (mixingRatio + 1))
    mtpPsPlot = time(DensityPlot.squareKernel(ps, window) * (mixingRatio / (mixingRatio + 1)), "squareKernel", false) // 3E5
    mergedPlot = time(if(ps.nonEmpty) mtpSketchPlot + mtpPsPlot else sketchPlot, "mergedPlot", false) // 2E6
    cmap = time(cmapForEqualSpaceCumulative(mergedPlot, cmapSize), "cmapForEqualSpaceCumulative", false) // 4E6
  } yield cmap

  def cmapForEqualSpaceCumulative(plot: DensityPlot, cmapSize: Int): Cmap = {
    val cdf = time(plot.cumulative, "cumulative", false) // 6E4
    val invCdf = time(cdf.inverse, "inverse", false) // 2E6
    val unit = cdf.interpolation(Double.MaxValue) / cmapSize.toDouble

    val divider = time((1 until cmapSize).toList.map(i => i * unit).map(a => invCdf.interpolation(a)), "divider", false) // 1E6
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