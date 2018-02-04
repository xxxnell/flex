package flip.pdf.update

import flip.conf.SmoothDistConf
import flip.pdf.{Count, Dist, NumericDist, PredefinedDist, Prim}
import flip.plot.DensityPlot
import flip.range.RangeP
import flip.measure.syntax._

object EqualSpaceSmoothingPs {

  def apply(ps: List[(Prim, Count)]): Dist[Prim] = {
    lazy val conf = SmoothDistConf.default
    lazy val plot = smoothingPsPlotForEqualSpaceCumulative(ps)

    if(ps.nonEmpty) {
      PredefinedDist.bare(doubleMeasure, conf, (start: Prim, end: Prim) => Some(plot.integral(start, end)))
    } else NumericDist.uniform(0.0, Double.PositiveInfinity)(doubleMeasure, conf)
  }

  def smoothingPsPlotForEqualSpaceCumulative(ps: List[(Prim, Count)]): DensityPlot = {
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
