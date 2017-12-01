package sketch.scope.plot

import sketch.scope.plot.CountPlot.{empty, modifyRecords}
import sketch.scope.range.Range

/**
  * Licensed by Probe Technology, Inc.
  */
trait DensityPlot extends Plot

trait DensityPlotOps extends PlotOps[DensityPlot] {

  def split(record: Record, p: Double): Option[(Record, Record)] = {
    val (range, value) = record

    if(range.start < p && range.end > p) {
      Option(((Range(range.start, p), value), (Range(p, range.end), value)))
    } else None
  }

}

trait DensityPlotSyntax {

  implicit class DensityPlotSyntaxImpl(dPlot: DensityPlot) extends PolyPlotSyntax[DensityPlot] {
    def plot: DensityPlot = dPlot
    def ops: PlotOps[DensityPlot] = DensityPlot
  }

}

object DensityPlot extends DensityPlotOps {

  case class DensityPlotImpl(records: List[Record]) extends DensityPlot

  private def bare(records: List[Record]): DensityPlot = DensityPlotImpl(records)

  def empty: DensityPlot = bare(Nil)

  def squareKernel(ds: List[Double], window: Double): DensityPlot =
    modifyRecords(DensityPlot.empty, _ => ds.map(d => (Range(d - (window / 2), d + (window / 2)), 1 / window)))

  def disjoint(records: List[Record]): DensityPlot = modifyRecords(empty, _ => records)

  def modifyRecords(plot: DensityPlot, f: List[Record] => List[Record]): DensityPlot =
    bare(planarize(f(plot.records)).map { case (range, values) => (range, values.sum / values.size) })

  def modifyValue(plot: DensityPlot, f: Record => Double): DensityPlot =
    bare(plot.records.map(record => (record._1, f(record))))

}
