package flip.plot

import flip.pdf.Prim
import flip.plot.syntax._
import flip.range._
import flip.range.syntax._
import org.apache.commons.math3.fitting.{PolynomialCurveFitter, WeightedObservedPoints}

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.language.postfixOps
import scala.math._
import scala.util.Try

/**
  * Disjoint and sorted set of range and value.
  */
trait Plot {

  def records: List[Record]

  lazy val samples: List[Block] = records
    .flatMap { case (range, value) => (range.start, value) :: (range.end, value) :: Nil }
    .sliding(2)
    .toList
    .flatMap {
      case p1 :: p2 :: Nil => Some((p1, p2))
      case _ => None
    }

  lazy val startIndexedBlocks: TreeMap[Prim, List[Block]] =
    TreeMap.apply(samples.groupBy { case ((start, _), _) => start }.toArray: _*)

  lazy val middleIndex: TreeMap[Prim, List[Block]] =
    TreeMap.apply(samples.groupBy { case ((start, _), (end, _)) => start / 2 + end / 2 }.toArray: _*)

  override def toString: String = {
    val recordsStr = records.map { case (range, value) => s"$range -> $value" }.mkString(", ")
    s"Plot($recordsStr)"
  }

}

trait PlotOps[P<:Plot] extends PlotLaws[P] {

  def modifyRecords(plot: P, f: List[Record] => List[Record]): P

  /**
    * Modify records without planarization.
    * It is only for performance enhancement, so please be careful to call this operation.
    * */
  private[plot] def unsafeModifyRecords(plot: P, f: List[Record] => List[Record]): P

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

    val midInterp = for {
      toIndexBlocks <- plot.middleIndex.to(x).lastOption
      fromIndexBlocks <- plot.middleIndex.from(x).headOption
      (toPoint, toBlocks) =  toIndexBlocks
      (fromPoint, fromBlocks) =  fromIndexBlocks
      toP = toBlocks.map { case ((x1, y1), (x2, y2)) => (x1 / 2 + x2 / 2, y1 / 2 + y2 / 2) }
        .maxBy { case (_, y) => y }
      fromP = fromBlocks.map { case ((x1, y1), (x2, y2)) => (x1 / 2 + x2 / 2, y1 / 2 + y2 / 2) }
        .minBy { case (_, y) => y }
      fitting <- dataFitting(toP :: fromP :: Nil, x)
    } yield fitting

    val headExt = plot.records.headOption.filter { case (range, _) => range.start >= x }
      .map { case (_, value) => value }

    val tailExt = plot.records.lastOption.filter { case (range, _) => range.end <= x }
      .map { case (_, value) => value }

    (midInterp orElse headExt orElse tailExt).getOrElse(0)
  }

  /**
    * @param as List of (x, y)
    * */
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
        fitting <- linearFitting(start, end, x)
      } yield fitting
    }
  }

  def linearFitting(a1: (Double, Double), a2: (Double, Double), x: Double): Option[Double] = {
    val (x1, y1) = a1
    val (x2, y2) = a2
    lazy val p = linearFittingDouble(a1, a2, x)

    if(!p.isNaN) Some(p)
    else try {
      Some(linearFittingBigDecimal(
        (BigDecimal(x1), BigDecimal(y1)),
        (BigDecimal(x2), BigDecimal(y2)),
        BigDecimal(x)
      ).toDouble)
    } catch {
      case _: Exception => None
    }
  }

  def linearFittingDouble(a1: (Double, Double), a2: (Double, Double), x: Double): Double = {
    val (x1, y1) = a1
    val (x2, y2) = a2

    if(y1 == y2) y1
    else if(x1 == x2) y1 + (y2 - y1) / 2
    else if(!x1.isInfinity && !x2.isInfinity) {
      lazy val slope1 = (y2 - y1) / (x2 - x1)
      lazy val slope2 = y2 * ((1 - y1 / y2) / (1 - x1 / x2)) / x2
      lazy val slope3 = y1 * ((1 - y2 / y1) / (1 - x2 / x1)) / x1
      lazy val slope4 = y1 / ((1 - x2 / x1) * x1)
      lazy val slope5 = y1 / ((x1 / x2 - 1) * x2)

      val slope =
        if(!slope1.isNaN && !slope1.isInfinity) slope1
        else if(!slope2.isNaN && !slope2.isInfinity) slope2
        else if(!slope3.isNaN && !slope3.isInfinity) slope3
        else if(!slope4.isNaN && !slope4.isInfinity && math.abs(y2 / y1) < 1 && x1 != 0) slope4
        else if(!slope5.isNaN && !slope5.isInfinity && math.abs(y2 / y1) < 1 && x2 != 0) slope5
        else Double.NaN
      val c = y1 - slope * x1

      val interp = slope * x + c

      if(!interp.isNaN && !interp.isInfinity) interp else Double.NaN
      // todo throw an exception when x is not sim to x1B
    } else Double.NaN
  }

  def linearFittingBigDecimal(a1: (BigDecimal, BigDecimal),
                              a2: (BigDecimal, BigDecimal),
                              x: BigDecimal): BigDecimal = {
    val (x1, y1) = a1
    val (x2, y2) = a2

    if(y1 == y2) y1
    else if(x1 == x2) y1 + (y2 - y1) / 2
    else {
      val slope = (y2 - y1) / (x2 - x1)
      val c = y1 - slope * x1

      slope * x + c
    }
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
    val boundaries: TreeSet[Double] =
      TreeSet(records.flatMap { case (range, _) => range.start :: range.end :: Nil }: _*)

    records
      .flatMap(record => planarizeRecord(record, boundaries))
      .groupBy { case (range, _) => range }
      .toList
      .map { case (range, grpRecords) => (range, grpRecords.map(_._2)) }
      .sortBy { case (range, _) => range.start }
  }

  def planarizeRecord(record: Record, boundaries: TreeSet[Double]): List[Record] = {
    val (_, planarized) = boundaries
      .from(record._1.start).to(record._1.end)
      .foldRight((record, List.empty[Record])){ case (b, (rem @ (range, _), acc)) =>
        if(range.start != b && range.end != b)
          split(rem, b).fold((rem, acc)){ case (rec1, rec2) => (rec1, rec2 :: acc) }
        else (rem, acc)
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
    unsafeModifyRecords(plot1, records => {
      val sumList = records ++ plot2.records
      planarizeRecords(sumList).map { case (range, values) => (range, values.sum) }
    })

  def multiplyConstant(plot: P, mag: Double): P = modifyValue(plot, { case (_, value) => value * mag })

  def integral(plot: P, start: Double, end: Double): Double = {
    lazy val startIndexedBlocksFromTo = plot.startIndexedBlocks.from(start).to(end)

    lazy val startBoundary: Double = (for {
      idxBlocks <- plot.startIndexedBlocks.to(start).lastOption
      (_, blocks) = idxBlocks
      ((x1, y1), (x2, y2)) = blocks
        .groupBy { case (_, (_x2, _)) => _x2 }.maxBy { case (_x2, _) => _x2 }._2
        .maxBy { case ((_, _y1), (_, _y2)) => _y1 / 2 + _y2 / 2 }
      yi = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(start)
    } yield if(start != x1) areaPoint(start, yi, x2, y2) else 0)
      .sum

    lazy val endBoundary: Double = (for {
      idxBlocks <- plot.startIndexedBlocks.to(end).lastOption
      (_, blocks) = idxBlocks
      ((x1, y1), (x2, y2)) = blocks
        .groupBy { case (_, (_x2, _)) => _x2 }.maxBy { case (_x2, _) => _x2 }._2
        .maxBy { case ((_, _y1), (_, _y2)) => _y1 / 2 + _y2 / 2 }
      yi = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(end)
    } yield areaPoint(x1, y1, end, yi))
      .sum

    lazy val mid: Double = (if(startIndexedBlocksFromTo.nonEmpty) {
      val endBlock = (for {
        idxBlocks <- startIndexedBlocksFromTo.lastOption
        (_, blocks) = idxBlocks
        block = blocks
          .groupBy { case (_, (_x2, _)) => _x2 }.maxBy { case (_x2, _) => _x2 }._2
          .maxBy { case ((_, _y1), (_, _y2)) => _y1 / 2 + _y2 / 2 }
      } yield areaBlock(block))
        .getOrElse(0.0)

      Some(startIndexedBlocksFromTo.values.toList.map(blocks => areaBlocks(blocks)).sum - endBlock)
    } else None)
      .sum

    lazy val startEndBoundary: Double = (if(startIndexedBlocksFromTo.isEmpty) {
      for {
        idxBlocks <- plot.startIndexedBlocks.to(end).lastOption
        (_, blocks) = idxBlocks
        block <-  blocks.find { case ((x1, _), (x2, _)) => x1 <= start && x2 >= end }
        ((x1, y1), (x2, y2)) = block
        plot = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil)
        yi1 = plot.interpolation(start)
        yi2 = plot.interpolation(end)
      } yield areaPoint(start, yi1, end, yi2)
    } else None)
    .sum

    if(startIndexedBlocksFromTo.nonEmpty) mid + startBoundary + endBoundary else startEndBoundary
  }

  def integralAll(plot: P): Double = domain(plot).map(range => integral(plot, range.start, range.end)).getOrElse(0.0)

  def areaPoint(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    if (y1 == 0 && y2 == 0) 0
    else (x2 - x1) * (y2 / 2 + y1 / 2)
  }

  def areaBlock(block: Block): Double = block match {
    case ((x1, y1), (x2, y2)) => areaPoint(x1, y1, x2, y2)
  }

  def areaBlocks(blocks: List[Block]): Double = blocks.map(block => areaBlock(block)).sum

  def isEmpty(plot: Plot): Boolean = plot.records.isEmpty

}

object Plot extends PlotOps[Plot] {

  // ops

  def modifyRecords(plot: Plot, f: List[Record] => List[Record]): Plot = plot match {
    case plot: DensityPlot => DensityPlot.modifyRecords(plot, f)
    case plot: CountPlot => CountPlot.modifyRecords(plot, f)
  }

  private[plot] def unsafeModifyRecords(plot: Plot, f: List[Record] => List[Record]): Plot = plot match {
    case plot: DensityPlot => DensityPlot.unsafeModifyRecords(plot, f)
    case plot: CountPlot => CountPlot.unsafeModifyRecords(plot, f)
  }

  def modifyValue(plot: Plot, f: Record => Double): Plot = plot match {
    case plot: DensityPlot => DensityPlot.modifyValue(plot, f)
    case plot: CountPlot => CountPlot.modifyValue(plot, f)
  }

  def split(record: Record, p: Double): Option[(Record, Record)] = DensityPlot.split(record, p)

}