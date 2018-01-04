package sketch.scope.plot

import org.apache.commons.math3.fitting.{PolynomialCurveFitter, WeightedObservedPoints}
import sketch.scope.RangeP
import sketch.scope.pdf.Prim
import sketch.scope.range._
import sketch.scope.range.syntax._
import sketch.scope.plot.syntax._

import scala.language.postfixOps
import scala.math._
import scala.util.Try

/**
  * Licensed by Probe Technology, Inc.
  *
  * Disjoint set of range and value
  */
trait Plot {

  def records: List[Record]

  override def toString: String = {
    val recordsStr = records.map { case (range, value) => s"$range -> $value" }.mkString(", ")
    s"Plot($recordsStr)"
  }

}

trait PlotOps[P<:Plot] extends PlotLaws[P] {

  def modifyRecords(plot: P, f: List[Record] => List[Record]): P

  def modifyValue(plot: P, f: Record => Double): P

  /**
    * @return Some of tuple (record with given range, remaining record) or None if <code>p</code> is not in record
    * */
  def split(record: Record, p: Double): Option[(Record, Record)]

}

trait PlotLaws[P<:Plot] { self: PlotOps[P] =>

  def inverse(plot: P): P = modifyRecords(plot, records => {
    records.map { case (range, value) => (RangeP.point(value), range.middle) }
  })

  def image(plot: P, argument: Double): Option[Double] = {
    plot.records.find { case (range, value) => range.contains(argument) }
      .map { case (range, value) => value }
  }

  def interpolation(plot: P, x: Double): Double = {
    val dataRefSize = 2

    val midInterp = plot.records.sliding(dataRefSize)
      .find { records => records.headOption.forall(_._1 <= x) && records.lastOption.forall(_._1 >= x) }
      .map(records => records.map { case (range, value) => (range.middle, value) })
      .flatMap { datas => dataFitting(datas, x) }

    val headExt = plot.records.headOption.filter { case (range, _) => range.start >= x }
      .map { case (_, value) => value }

    val tailExt = plot.records.lastOption.filter { case (range, _) => range.end <= x }
      .map { case (_, value) => value }

    (midInterp orElse headExt orElse tailExt).getOrElse(0)
  }

  def dataFitting(as: List[(Double, Double)], x: Double): Option[Double] = {
    val largeCutoff = 1e300
    val smallCutoff = -1e300

    val polyValid = as.forall { case (_x, _y) =>
      _x < largeCutoff && _y < largeCutoff && _x > smallCutoff && _y > smallCutoff
    }

    (as.size, polyValid) match {
      case (size, true) if size > 2 =>
        polynomialFitting(as, x)
      case _ => for {
        start <- as.find(_._1 <= x)
        end <- as.find(_._1 >= x)
      } yield linearFitting(start, end, x)
    }
  }

  def linearFitting(a1: (Double, Double), a2: (Double, Double), x: Double): Double = {
    val (x1, y1) = a1
    val (x2, y2) = a2
    val (x1B, y1B) = (BigDecimal(x1), BigDecimal(y1))
    val (x2B, y2B) = (BigDecimal(x2), BigDecimal(y2))

    if (x2B != x1B) {
      val slope = (y2B - y1B) / (x2B - x1B)
      val c = y1B - slope * x1B

      (slope * x + c).toDouble
    } else ((y1B + y2B) / 2).toDouble // todo throw an exception when x is not sim to x1B
  }

  def polynomialFitting(as: List[(Double, Double)], x: Double): Option[Double] = Try {
    val xB = BigDecimal(x)

    val obs = as.foldLeft(new WeightedObservedPoints){ case (_obs, (_x, _y)) => _obs.add(_x, _y); _obs }
    val fitter = PolynomialCurveFitter.create(2)
    val coeff = fitter.fit(obs.toList)

    (coeff(0) * xB * xB + coeff(1) * xB + coeff(2)).toDouble
  }.toOption

  /**
    * Plalarize records that is transforming overlapped records to disjointed records.
    * e.g. {[0..2] -> 1, [1..3] -> 2} => {[0..1] -> {split(1)}, [1..2] -> {split(1), split(2)}, [2..3] -> split(2)}
    * */
  def planarizeRecords(records: List[Record]): List[(RangeP, List[Double])] = {
    val boundaries: Vector[Double] =
      records.flatMap { case (range, _) => range.start :: range.end :: Nil }.sorted.toVector

    records
      .flatMap { record => planarizeRecord(record, boundaries) }
      .groupBy { case (range, _) => range }
      .toList
      .map { case (range, grpRecords) => (range, grpRecords.map(_._2)) }
      .sortBy { case (range, _) => range.start }
  }

  def planarizeRecord(record: Record, boundaries: Vector[Double]): List[Record] = {
    val (_, planarized) = boundaries
      .filter(b => record._1.start < b && record._1.end > b)
      .foldRight((record, List.empty[Record])){ case (b, (rem, acc)) =>
        split(rem, b).fold((rem, acc)){ case (rec1, rec2) => (rec1, rec2 :: acc) }
      }

    if(planarized.nonEmpty) planarized else List(record)
  }

  def domain(plot: P): Option[RangeP] = {
    val startO = Try(plot.records.map { case (range, _) => range.start }.min).toOption
    val endO = Try(plot.records.map { case (range, _) => range.end }.max).toOption

    for {
      start <- startO
      end <- endO
    } yield RangeP(start, end)
  }

  def add(plot1: P, plot2: P): P =
    modifyRecords(plot1, records =>
      planarizeRecords(records ++ plot2.records).map { case (range, values) => (range, values.sum) }
    )

  def multiplyConstant(plot: P, mag: Double): P = modifyValue(plot, { case (_, value) => value * mag })

  def integral(plot: P, start: Double, end: Double): Double = {
    val samples: List[(Prim, Double)] = plot.records
      .flatMap { case (range, value) => (range.start, value) :: (range.end, value) :: Nil }

    val slidings = samples.sliding(2).toList.flatMap {
      case s1 :: s2 :: Nil => Some((s1, s2))
      case _ => None
    }

    val mid: Double = slidings
      .filter { case ((x1, _), (x2, _)) => x1 > start && x2 < end }
      .map { case ((x1, y1), (x2, y2)) => area(x1, y1, x2, y2) }
      .sum

    val startBoundary: Double = slidings
      .filter { case ((x1, _), (x2, _)) => RangeP(x1, x2).contains(start) }
      .map { case ((x1, y1), (x2, y2)) =>
        val yi = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(start)
        area(start, yi, x2, y2)
      }.sum

    val endBoundary: Double = slidings
      .filter { case ((x1, _), (x2, _)) => RangeP(x1, x2).contains(end) }
      .map { case ((x1, y1), (x2, y2)) =>
        val yi = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(end)
        area(x1, y1, end, yi)
      }.sum

    val startEndBoundary: Double = slidings
      .filter { case ((x1, _), (x2, _)) => RangeP(x1, x2).contains(start) && RangeP(x1, x2).contains(end) }
      .map { case ((x1, y1), (x2, y2)) =>
        val yi1 = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(start)
        val yi2 = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(end)
        area(start, yi1, end, yi2)
      }.sum

    if(startEndBoundary == 0) mid + startBoundary + endBoundary else startEndBoundary
  }

  def area(x1: Double, y1: Double, x2: Double, y2: Double): Double = (x2 - x1) * (y2 + y1) / 2

  def isEmpty(plot: Plot): Boolean = plot.records.isEmpty

}

trait PlotSyntax {

  type Record = (RangeP, Double)

  implicit class PlotSyntaxImpl(plot: Plot) {
    def image(argument: Double): Option[Double] = Plot.image(plot, argument)
    def value(argument: Double): Option[Double] = Plot.image(plot, argument)
    def interpolation(argument: Double): Double = Plot.interpolation(plot, argument)
    def integral(start: Double, end: Double): Double = Plot.integral(plot, start, end)
    def isEmpty: Boolean = Plot.isEmpty(plot)
    def nonEmpty: Boolean = !Plot.isEmpty(plot)
  }

}

trait PolyPlotSyntax[P<:Plot] {
  // context
  def plot: P
  def ops: PlotOps[P]
  // syntax
  def modify(f: Record => Double): P = ops.modifyValue(plot, f)
  def add(plot2: P): P = ops.add(plot, plot2)
  def +(plot2: P): P = ops.add(plot, plot2)
  def multiply(mag: Double): P = ops.multiplyConstant(plot, mag)
  def *(mag: Double): P = ops.multiplyConstant(plot, mag)
  def inverse: P = ops.inverse(plot)
  def domain: Option[RangeP] = ops.domain(plot)
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