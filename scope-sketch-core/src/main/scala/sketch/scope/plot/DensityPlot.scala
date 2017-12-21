package sketch.scope.plot

import sketch.scope.pdf._
import sketch.scope.range.RangeP

/**
  * Licensed by Probe Technology, Inc.
  */
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

    modifyRecords(plot, (records: List[Record]) => {
      val utdRecord = records.flatMap { case (range, value) =>
        val startVal = accVal
        val endVal = (accVal + value * range.length).toDouble
        accVal = endVal
        (RangeP.point(range.start), startVal) :: (RangeP.point(range.end), endVal) :: Nil
      }
      println("cumulative: " + utdRecord.headOption.fold(utdRecord)(utdHead => if(utdHead._1 != cumHead._1) cumHead :: utdRecord else utdRecord))
      utdRecord.headOption.fold(utdRecord)(utdHead => if(utdHead._1 != cumHead._1) cumHead :: utdRecord else utdRecord)
    })
  }

}

trait DensityPlotSyntax {

  implicit class DensityPlotSyntaxImpl(dPlot: DensityPlot) extends PolyPlotSyntax[DensityPlot] {
    def plot: DensityPlot = dPlot
    def ops: PlotOps[DensityPlot] = DensityPlot

    def cumulative: DensityPlot = DensityPlot.cumulative(plot)
  }

}

object DensityPlot extends DensityPlotOps {

  case class DensityPlotImpl(records: List[Record]) extends DensityPlot

  private def bare(records: List[Record]): DensityPlot = DensityPlotImpl(records)

  def empty: DensityPlot = bare(Nil)

  def squareKernel(ds: List[(Prim, Count)], window: Double): DensityPlot = {
    val sum = ds.map(d => d._2).sum

    modifyRecords(DensityPlot.empty, _ => ds.map { case (value, count) =>
      (RangeP(value - (window / 2), value + (window / 2)), count / (sum * window))
    })
  }

  def disjoint(records: List[Record]): DensityPlot = modifyRecords(empty, _ => records)

  def modifyRecords(plot: DensityPlot, f: List[Record] => List[Record]): DensityPlot =
    bare(planarizeRecords(f(plot.records)).flatMap { case (range, values) => values.map(value => (range, value)) })
//    bare(planarizeRecords(f(plot.records)).map { case (range, values) => (range, values.sum / values.size) })

  def modifyValue(plot: DensityPlot, f: Record => Double): DensityPlot =
    bare(plot.records.map(record => (record._1, f(record))))

}
