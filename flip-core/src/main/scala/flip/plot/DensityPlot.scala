package flip.plot

import cats.data.NonEmptyList
import flip.pdf._
import flip.range._
import flip.range.syntax._
import flip.plot.syntax._
import flip.range

trait DensityPlot extends RangePlot

trait DensityPlotOps extends RangePlotOps[DensityPlot] {

  def split(record: Record, p: Double): Option[(Record, Record)] = {
    val (range, value) = record

    if (range.contains(p)) {
      Option(((RangeP(range.start, p), value), (RangeP(p, range.end), value)))
    } else None
  }

}

object DensityPlot extends DensityPlotOps {

  case class DensityPlotImpl(records: List[Record]) extends DensityPlot

  private def bare(records: List[Record]): DensityPlot = DensityPlotImpl(records)

  def empty: DensityPlot = bare(Nil)

  def squareKernel(ds: List[(Prim, Count)], window: Double): DensityPlot = {
    val sum = ds.map(d => d._2).sum
    val utdWindow = if (window <= 0) 1e-100 else window

    modifyRecords(
      DensityPlot.empty,
      _ =>
        ds.map {
          case (value, count) =>
            (
              RangeP(value - (utdWindow / 2), value + (utdWindow / 2)),
              if (sum * utdWindow > 0) count / (sum * utdWindow) else 0)
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
