package sketch.scope.plot

import sketch.scope.range._
import org.apache.commons.math3.fitting.{PolynomialCurveFitter, WeightedObservedPoints}

import scala.collection.immutable.{HashSet, Set}
import scala.language.postfixOps
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

  def interpolation(plot: P, argument: Double): Double = {
    val dataRefSize = 2

    val midInterp = plot.records.sliding(dataRefSize)
      .find { records =>
        records.headOption.flatMap { case (headRange, _) => records.lastOption.map { case (lastRange, _) =>
          RangeP(headRange.start, lastRange.end).contains(argument)
        }}.getOrElse(false)
      }.map(records => records.map { case (range, value) => (range.middle, value) })
      .flatMap { datas => dataFitting(datas, argument) }

    val headExt = plot.records.headOption.filter { case (range, _) => range.start >= argument }
      .map { case (_, value) => value }

    val tailExt = plot.records.lastOption.filter { case (range, _) => range.end <= argument }
      .map { case (_, value) => value }

//    println(s"argument: $argument, result: ${(midInterp orElse headExt orElse tailExt).getOrElse(0)}")

    (midInterp orElse headExt orElse tailExt).getOrElse(0)
  }

  def list2Triplet[A](list: List[A]): Option[(A, A, A)] = list match {
    case List(a, b, c) => Some((a, b, c))
    case _ => None
  }

  def dataFitting(as: List[(Double, Double)], x: Double): Option[Double] = {
    val largeCutoff = 1e300
    val smallCutoff = -1e300

    val polyValid = as.forall { case (_x, _y) =>
      _x < largeCutoff && _y < largeCutoff && _x > smallCutoff && _y > smallCutoff
    }

    (as.size, polyValid) match {
      case (size, true) if size > 2 => polynomialFitting(as, x)
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

    val slope = (y2B - y1B) / (x2B - x1B)
    val c = y1B - slope * x1B

    (slope * x + c).toDouble
  }

  def polynomialFitting(as: List[(Double, Double)], x: Double): Option[Double] = Try {
    val xB = BigDecimal(x)

//    println(s"as: $as, x: $x")

    val obs = as.foldLeft(new WeightedObservedPoints){ case (_obs, (_x, _y)) => _obs.add(_x, _y); _obs }
    val fitter = PolynomialCurveFitter.create(2)
    val coeff = fitter.fit(obs.toList)

    println(s"as: $as, x: $x, coeff: ${coeff.toList}, result: ${(coeff(0) * xB * xB + coeff(1) * xB + coeff(2)).toDouble}")

    (coeff(0) * xB * xB + coeff(1) * xB + coeff(2)).toDouble
  }.toOption

  /**
    * Plalarize records that is transforming overlapped records to disjointed records.
    * e.g. {[0..2] -> 1, [1..3] -> 2} => {[0..1] -> {split(1)}, [1..2] -> {split(1), split(2)}, [2..3] -> split(2)}
    * */
  def planarizeRecords(records: List[Record]): List[(RangeP, List[Double])] = {
    val boundaries: Vector[Double] =
      records.flatMap { case (range, _) => range.start :: range.end :: Nil }.sorted.toVector

//    println("records: " + records)
//    println("planarizeRecord: " + records.flatMap { record => planarizeRecord(record, boundaries) })

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

  def remove[A](xs: Vector[A], a: A): Vector[A] = {
    val idx = xs.indexOf(a)
    (xs take idx) ++ (xs drop (idx + 1))
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