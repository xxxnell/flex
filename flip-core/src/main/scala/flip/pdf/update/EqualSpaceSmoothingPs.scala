package flip.pdf.update

import flip.conf.SmoothDistConf
import flip.pdf.{Count, Dist, NumericDist, PredefinedDist, Prim}
import flip.plot.DensityPlot
import flip.range.RangeP
import flip.measure.syntax._

object EqualSpaceSmoothingPs extends SmoothingPs {

  def apply(ps: List[(Prim, Count)], scale: Double): Dist[Prim] = {
    lazy val conf = SmoothDistConf.default
    lazy val plot = smoothingPsPlotForEqualSpaceCumulative(ps)
    lazy val cdf = plot.normalizedCumulative
    def probability(start: Prim, end: Prim): Double = cdf.interpolation(end) - cdf.interpolation(start)

    if (ps.nonEmpty) {
      PredefinedDist.probability((start: Prim, end: Prim) => probability(start, end))
    } else NumericDist.uniform(0.0, Double.PositiveInfinity)(doubleMeasure, conf)
  }

  def smoothingPsPlotForEqualSpaceCumulative(ps: List[(Prim, Count)]): DensityPlot = {
    val sum = ps.map(_._2).sum
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

    val densityRecords = (headAppendingO.toList ::: sorted ::: lastAppendingO.toList)
      .sliding(2)
      .toList
      .flatMap {
        case (p1, count1) :: (p2, count2) :: Nil if !p1.isInfinity && !p2.isInfinity =>
          val range = RangeP(p1, p2)
          if (!range.isPoint) Some((range, (count1 + count2) / (2 * range.length).toDouble)) else None
        case _ => None
      }
      .map { case (range, count) => (range, count / sum) }

    DensityPlot.disjoint(densityRecords)
  }

}
