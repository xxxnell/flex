package sketch.scope.dist.update

import sketch.scope.cmap.Cmap
import sketch.scope.dist.{Prim, Range, Ranges, Sketch}
import sketch.scope.hcounter.HCounter

import scala.util.Try

/**
  * Licensed by Probe Technology, Inc.
  */
trait UniformCdfUpdate {

  def updateCmap(sketch: Sketch[_], ps: List[Prim]): Option[Cmap] = ???

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