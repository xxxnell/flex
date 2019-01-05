package flex.plot

import flex.plot.syntax._
import flex.range
import flex.range.RangeP

trait CountPlot extends RangePlot

trait CountPlotOps extends RangePlotOps[CountPlot] {

  def split(record: Record, p: Double): Option[(Record, Record)] = {
    val (range, value) = record

    if (range.contains(p)) {
      lazy val value1B = if (range.length != BigDecimal(0)) (((p - range.start) / range.length) * value).toDouble else 0
      val value1 =
        if (p != 0 && range.end != 0 && range.start / range.end != 1) {
          val v = (p / range.end) * (1 - range.start / p) / (1 - range.start / range.end)
          if (!v.isNaN) v else value1B
        } else if (!range.isPoint) {
          value1B
        } else 0
      val value2 = value - value1

      Option(((RangeP(range.start, p), value1), (RangeP(p, range.end), value2)))
    } else None
  }

}

object CountPlot extends CountPlotOps {

  case class CountPlotImpl(records: List[Record]) extends CountPlot

  private def bare(records: List[Record]): CountPlot = CountPlotImpl(records)

  def empty: CountPlot = bare(Nil)

  def disjoint(records: List[Record]): CountPlot = modifyRecords(empty, _ => records)

  def modifyRecords(plot: CountPlot, f: List[Record] => List[Record]): CountPlot =
    bare(planarizeRecords(f(plot.records)).map { case (range, values) => (range, values.sum / values.size) })

  private[plot] def unsafeModifyRecords(plot: CountPlot, f: List[Record] => List[Record]): CountPlot =
    bare(f(plot.records))

  def modifyValue(plot: CountPlot, f: Record => Double): CountPlot =
    bare(plot.records.map(record => (record._1, f(record))))

}
