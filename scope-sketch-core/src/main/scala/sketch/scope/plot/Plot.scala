package sketch.scope.plot

import sketch.scope.range._
import org.apache.commons.math3.fitting.{PolynomialCurveFitter, WeightedObservedPoints}

import scala.collection.immutable.{HashSet, Set}

/**
  * Licensed by Probe Technology, Inc.
  *
  * Disjoint set of range and value
  */
trait Plot {

  def records: List[Record]

}

trait PlotOps[P<:Plot] extends PlotLaws[P] {

  def modifyRecords(plot: P, f: List[Record] => List[Record]): P

  def modifyValue(plot: P, f: Record => Double): P

  /**
    * @return Some of tuple (record with given range, remaining record) or None if <code>p</code> is not in record
    * */
  def split(record: Record, p: Double): Option[(Record, Record)]

  def inverse(plot: P): P = modifyRecords(plot, records => {
    records.map { case (range, value) => (RangeP.point(value), range.middle) }
  })

  def cumulative(plot: P): P = {
    var accVal = 0.toDouble

    modifyRecords(plot, (records: List[Record]) => records.map { case (range, value) =>
      accVal = accVal + value
      (range, accVal)
    })
  }

  def image(plot: P, argument: Double): Option[Double] = {
    plot.records.find { case (range, value) => range.contains(argument) }
      .map { case (range, value) => value }
  }

  def interpolation(plot: P, argument: Double): Double = {
    val midInterp = plot.records.sliding(3).flatMap(recordGroup => list2Triplet(recordGroup))
      .find { case ((range1, _), _, (range2, _)) => range1.start < argument && range2.end > argument }
      .map { case ((range1, value1), (range2, value2), (range3, value3)) =>
        ((range1.middle, value1), (range2.middle, value2), (range3.middle, value3))
      }.map { case (a1, a2, a3) => polynomialFitting(a1 :: a2 :: a3 :: Nil, argument) }

    val headExt = plot.records.headOption.filter { case (range, _) => range.start > argument }
      .map { case (_, value) => value }

    val tailExt = plot.records.lastOption.filter { case (range, _) => range.end < argument }
      .map { case (_, value) => value }

    (midInterp orElse headExt orElse tailExt).getOrElse(0)
  }

  def list2Triplet[A](list: List[A]): Option[(A, A, A)] = list match {
    case List(a, b, c) => Some((a, b, c))
    case _ => None
  }

  def polynomialFitting(as: List[(Double, Double)], x: Double): Double = {
    val obs = as.foldLeft(new WeightedObservedPoints){ case (_obs, (_x, _y)) => _obs.add(_x, _y); _obs }
    val fitter = PolynomialCurveFitter.create(2)
    val coeff = fitter.fit(obs.toList)

    coeff(0) * x * x + coeff(1) * x + coeff(2)
  }

  def planarize(records: List[Record]): List[(RangeP, List[Double])] = {
    val boundaries: HashSet[Double] =
      HashSet(records.flatMap { case (range, _) => range.start :: range.end :: Nil }.sorted.toArray: _*)

    records
      .flatMap { record => planarizeRecord(record, boundaries) }
      .groupBy { case (range, _) => range }
      .toList
      .map { case (range, grpRecords) => (range, grpRecords.map(_._2)) }
      .sortBy { case (range, _) => range.start }
  }

  def planarizeRecord(record: Record, boundaries: Set[Double]): List[Record] = {
    def planarizeRecordAcc(record: Record, boundaries: Set[Double], acc: List[Record]): List[Record] = {
      boundaries.find(b => record._1.start < b && record._1.end > b) match {
        case Some(boundary) => split(record, boundary) match {
          case Some((splitted, rem)) => planarizeRecordAcc(rem, boundaries, splitted :: acc)
          case None => acc
        }
        case None => acc
      }
    }

    planarizeRecordAcc(record, boundaries, Nil)
  }

}

trait PlotLaws[P<:Plot] { self: PlotOps[P] =>

  def add(plot1: P, plot2: P): P =
    modifyRecords(plot1, records =>
      planarize(records ++ plot2.records).map { case (range, values) => (range, values.sum) }
    )

  def multiply(plot: P, mag: Double): P = modifyValue(plot, record => record._2 * mag)

}

trait PlotSyntax {

  implicit class PlotSyntaxImpl(plot: Plot) {
    def image(argument: Double): Option[Double] = Plot.image(plot, argument)
    def value(argument: Double): Option[Double] = Plot.image(plot, argument)
    def interpolation(argument: Double): Double = Plot.interpolation(plot, argument)
  }

}

trait PolyPlotSyntax[P<:Plot] {
  def plot: P
  def ops: PlotOps[P]
  def modify(f: Record => Double): P = ops.modifyValue(plot, f)
  def add(plot2: P): P = ops.add(plot, plot2)
  def +(plot2: P): P = ops.add(plot, plot2)
  def multiply(mag: Double): P = ops.multiply(plot, mag)
  def *(mag: Double): P = ops.multiply(plot, mag)
  def inverse: P = ops.inverse(plot)
  def cumulative: P = ops.cumulative(plot)
}

object Plot extends PlotOps[Plot] {

  // ops

  def modifyRecords(plot: Plot, f: List[Record] => List[Record]): Plot = plot match {
    case plot: DensityPlot => DensityPlot.modifyRecords(plot, f)
    case plot: CountPlot => CountPlot.modifyRecords(plot, f)
  }

  def modifyValue(plot: Plot, f: Record => Double): Plot = plot match {
    case plot: DensityPlot => DensityPlot.modifyValue(plot, f)
    case plot: CountPlot => CountPlot.modifyValue(plot, f)
  }

  def split(record: Record, p: Double): Option[(Record, Record)] = DensityPlot.split(record, p)

}