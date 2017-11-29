package sketch.scope.pdf.update

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.pdf.{Prim, Sketch}
import sketch.scope.plot._

/**
  * Licensed by Probe Technology, Inc.
  */
trait UniformCdfUpdate {

  def updateCmap(sketch: Sketch[_], ps: List[Prim], mixingRate: Double, window: Double): Option[Cmap] = for {
    sketchPlot <- sketch.densityPlot
    mtpSketchPlot = sketchPlot * (1 / (mixingRate + 1))
    mtpPsPlot = Plot.squareKernel(ps, window) * (mixingRate / (mixingRate + 1))
    mergedPlot = mtpSketchPlot + mtpPsPlot
    caSize <- sketch.structures.headOption.map { case (cmap, _) => cmap.size }
    cmap = cmapForUniformSplitCumulative(mergedPlot, caSize)
  } yield cmap

  def cmapForUniformSplitCumulative(plot: Plot, caSize: Int): Cmap = {
    val cdf = plot.cumulative
    val invCdf = cdf.inverse
    val unit = cdf.value(Double.MaxValue) / caSize.toDouble

    val divider = (1 until caSize).toList.map(i => i * unit).map(a => invCdf.value(a))
    Cmap.divider(divider)
  }

  //  def rearrangeCmap(sketch: Sketch[_]): Option[Cmap] = for {
//    densityPlot <- densityPlot(sketch)
//    size <- sketch.structure.headOption.map { case (cmap, _) => cmap.size }
//    utdRanges = updateRanges(size, densityPlot)
//    divider = utdRanges.map(_.start).drop(1)
//  } yield Cmap.divider(divider)
//
//  /**
//    * @return (updated ranges, remaining accumulation)
//    * */
//  def updateRanges(size: Int, plot: List[(Range, Double)]): Ranges = {
//    def updateRangesAcc(size:Int, plot: List[(Range, Double)], acc: Ranges): Ranges = {
//      val unit = 1 / size.toDouble
//      if(size > 0) {
//        val splitted = split(unit, plot)
//        val rangeO = for {
//          head <- splitted._1.headOption
//          last <- splitted._1.lastOption
//        } yield head._1.start to last._1.end
//
//        rangeO match {
//          case None => acc.reverse
//          case Some(range) => updateRangesAcc(size - 1, splitted._2, range.by(1) :: acc)
//        }
//      } else acc.reverse
//    }
//
//    updateRangesAcc(size, plot, Nil)
//  }
//
//  def split(area: Double, plot: List[(Range, Double)]): (List[(Range, Double)], List[(Range, Double)]) = {
//    def splitAcc(area: Double,
//                 plot: List[(Range, Double)],
//                 acc: (List[(Range, Double)], List[(Range, Double)])):
//    (List[(Range, Double)], List[(Range, Double)]) = {
//      plot match {
//        case Nil => acc
//        case (range, density) :: tail =>
//          val segArea = (range.end - range.start) * density
//          if (segArea >= area) {
//            val mid = (area / density) + range.start
//            (acc._1 :+ ((range.start to mid).by(1), density), ((mid to range.end).by(1), density) :: acc._2.drop(1))
//          } else splitAcc(area - segArea, tail, (acc._1 :+ (range, density), acc._2.drop(1)))
//      }
//    }
//
//    splitAcc(area, plot, (Nil, plot))
//  }

}

object UniformCdfUpdate extends UniformCdfUpdate