package flip.plot

import cats.data.NonEmptyList
import flip.pdf._
import flip.range._
import flip.range.syntax._
import flip.plot.syntax._
import flip.range

trait DensityPlot extends Plot

trait DensityPlotOps extends PlotOps[DensityPlot] {

  def split(record: Record, p: Double): Option[(Record, Record)] = {
    val (range, value) = record

    if(range.contains(p)) {
      Option(((RangeP(range.start, p), value), (RangeP(p, range.end), value)))
    } else None
  }

  def cumulative(plot: DensityPlot): DensityPlot = {
    var accVal = 0d
    val cumHead = (RangeP.point(Double.MinValue), 0d)

    unsafeModifyRecords(plot, (records: List[Record]) => {
      val utdRecord = records.flatMap { case (range, value) =>
        val startVal = accVal
        val endVal = accVal + value * range.roughLength
        accVal = endVal

        (RangeP.point(range.start), startVal) :: (RangeP.point(range.end), endVal) :: Nil
      }

      utdRecord.headOption.fold(utdRecord)(utdHead => if(utdHead._1 != cumHead._1) cumHead :: utdRecord else utdRecord)
    })
  }

  def inverseCumulative(plot: DensityPlot): DensityPlot = {
    unsafeModifyRecords(cumulative(plot), (records: List[Record]) =>
      records.map { case (range, value) => (RangeP.point(value), range.middle) }
    )
  }

  def weightedAdd(wps: NonEmptyList[(Double, DensityPlot)]): DensityPlot = {
    val listWps = wps.toList
    val norm = listWps.map(_._1).sum
    val normWps = listWps.map { case (weight, plot) =>
      val normWeight = if(norm != 0) weight / norm else 1 / listWps.size.toDouble
      (normWeight, plot)
    }

    val concatRecords: List[Record] = normWps.map { case (weight, plot) =>
      plot.records.map { case (range, value) => (range, weight * value) }
    }.foldLeft(List.empty[Record]){ case (acc, rec) => acc ++ rec }

    val mgdRecords = planarizeRecords(concatRecords).map { case (range, values) => (range, values.sum) }

    unsafeModifyRecords(wps.head._2, _ => mgdRecords)
  }

}

object DensityPlot extends DensityPlotOps {

  case class DensityPlotImpl(records: List[Record]) extends DensityPlot

  private def bare(records: List[Record]): DensityPlot = DensityPlotImpl(records)

  def empty: DensityPlot = bare(Nil)

  def squareKernel(ds: List[(Prim, Count)], window: Double): DensityPlot = {
    val sum = ds.map(d => d._2).sum
    val utdWindow = if(window <= 0) 1e-100 else window

    modifyRecords(DensityPlot.empty, _ => ds.map { case (value, count) =>
      (RangeP(value - (utdWindow / 2), value + (utdWindow / 2)),
        if(sum * utdWindow > 0) count / (sum * utdWindow) else 0)
    })
  }

  def disjoint(records: List[Record]): DensityPlot = modifyRecords(empty, _ => records)

  def modifyRecords(plot: DensityPlot, f: List[Record] => List[Record]): DensityPlot = {
    bare(planarizeRecords(f(plot.records)).flatMap { case (range, values) => values.map(value => (range, value)) })
  }

  private[plot] def unsafeModifyRecords(plot: DensityPlot, f: List[Record] => List[Record]): DensityPlot = {
    bare(f(plot.records))
  }

  def modifyValue(plot: DensityPlot, f: Record => Double): DensityPlot =
    bare(plot.records.map(record => (record._1, f(record))))

}
