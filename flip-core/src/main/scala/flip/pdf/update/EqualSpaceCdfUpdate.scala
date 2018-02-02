package flip.pdf.update

import flip.cmap.Cmap
import flip.pdf._
import flip.plot._
import flip.plot.syntax._
import flip.range.RangeP

trait EqualSpaceCdfUpdate {

  def updateCmapForSketch(sketch: Sketch[_], ps: List[(Prim, Count)]): Option[Cmap] = for {
    sketchSamples <- sketch.sampling
    mixingRatio = sketch.conf.mixingRatio
    window = sketch.conf.dataKernelWindow
    corr = sketch.conf.boundaryCorrection
    cmapSize = sketch.conf.cmap.size
  } yield updateCmap(sketchSamples, ps, mixingRatio, window, corr, cmapSize)

  def updateCmap(sketchSamples: DensityPlot, ps: List[(Prim, Count)],
                 mixingRatio: Double, window: Double, corr: Double, cmapSize: Int): Cmap = {
    val mtpSketchSamples = sketchSamples * (1 / (mixingRatio + 1))
    val mtpPsPlot = DensityPlot.squareKernel(ps, window) * (mixingRatio / (mixingRatio + 1))
    val mergedPlot = if(ps.nonEmpty) mtpSketchSamples + mtpPsPlot else sketchSamples

    cmapForEqualSpaceCumCorr(mergedPlot, corr, cmapSize)
  }

  /**
    * @param corr boundary marginal ratio for the separation unit.
    *             If corr=1, cmapForEqualSpaceCumCorr is identical to standard cmapForEqualSpaceCum.
    *             If corr=0, cmap has no margin.
    * */
  def cmapForEqualSpaceCumCorr(plot: DensityPlot, corr: Double, cmapSize: Int): Cmap = {
    lazy val cdf = plot.cumulative
    lazy val invCdf = cdf.inverse

    val cdfDivider = if(cmapSize < 2) {
      Nil
    } else if(cmapSize == 2) {
      0.5 :: Nil
    } else {
      val unit = cdf.interpolation(Double.MaxValue) / (cmapSize.toDouble - 2 + 2 * corr)

      (1 until cmapSize).toList
        .map(i => unit * corr + unit * (i - 1))
    }

    val pDivider = cdfDivider.map(a => invCdf.interpolation(a))

    Cmap.divider(pDivider)
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