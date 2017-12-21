package sketch.scope.plot

import sketch.scope.plot.DensityPlot.{bare, modifyRecords, planarizeRecords}
import sketch.scope.range.RangeP

/**
  * Licensed by Probe Technology, Inc.
  */
trait CountPlot extends Plot

trait CountPlotOps extends PlotOps[CountPlot] {

  def split(record: Record, p: Double): Option[(Record, Record)] = {
    val (range, value) = record

    if(range.contains(p)) {
      val value1 = if(range.length != 0) (((p - range.start) / range.length) * value).toDouble else 0
      val value2 = value - value1

      Option(((RangeP(range.start, p), value1), (RangeP(p, range.end), value2)))
    } else None
  }

}

trait CountPlotSyntax {

  implicit class CountPlotSyntaxImpl(cPlot: CountPlot) extends PolyPlotSyntax[CountPlot] {
    def plot: CountPlot = cPlot
    def ops: PlotOps[CountPlot] = CountPlot
  }

}

object CountPlot extends CountPlotOps {

  case class CountPlotImpl(records: List[Record]) extends CountPlot

  private def bare(records: List[Record]) : CountPlot = CountPlotImpl(records)

  def empty: CountPlot = bare(Nil)

  def disjoint(records: List[Record]): CountPlot = modifyRecords(empty, _ => records)

  def modifyRecords(plot: CountPlot, f: List[Record] => List[Record]): CountPlot = {
    bare(planarizeRecords(f(plot.records)).map { case (range, values) => (range, values.sum / values.size) })
  }

  def modifyValue(plot: CountPlot, f: Record => Double): CountPlot =
    bare(plot.records.map(record => (record._1, f(record))))

}
