package flip.plot

import cats.data.NonEmptyList
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
  * Plot is disjoint and sorted set of range and value.
  */
trait RangePlot extends Plot {

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

trait RangePlotOps[P <: RangePlot] extends PlotOps[P] with RangePlotLaws[P] {

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

trait RangePlotLaws[P <: RangePlot] { self: RangePlotOps[P] =>

  def inverse(plot: P): P =
    modifyRecords(plot, records => {
      records.map { case (range, value) => (RangeP.point(value), range.middle) }
    })

  def image(plot: P, argument: Double): Option[Double] = {
    plot.records
      .find { case (range, value) => range.contains(argument) }
      .map { case (range, value) => value }
  }

  def interpolation(plot: P, x: Double): Double = {
    lazy val midInterp = for {
      toIndexBlocks <- plot.middleIndex.to(x).lastOption
      fromIndexBlocks <- plot.middleIndex.from(x).headOption
      (toPoint, toBlocks) = toIndexBlocks
      (fromPoint, fromBlocks) = fromIndexBlocks
      toP = toBlocks
        .map { case ((x1, y1), (x2, y2)) => (x1 / 2 + x2 / 2, y1 / 2 + y2 / 2) }
        .maxBy { case (_, y) => y }
      fromP = fromBlocks
        .map { case ((x1, y1), (x2, y2)) => (x1 / 2 + x2 / 2, y1 / 2 + y2 / 2) }
        .minBy { case (_, y) => y }
      fitting <- Fitting.dataFitting(toP :: fromP :: Nil, x)
    } yield fitting

    lazy val headExt = plot.records.headOption
      .filter { case (range, _) => range.start >= x }
      .map { case (_, value) => value }

    lazy val tailExt = plot.records.lastOption
      .filter { case (range, _) => range.end <= x }
      .map { case (_, value) => value }

    (midInterp orElse headExt orElse tailExt).getOrElse(0)
  }

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
      .from(record._1.start)
      .to(record._1.end)
      .foldRight((record, List.empty[Record])) {
        case (b, (rem @ (range, _), acc)) =>
          if (range.start != b && range.end != b)
            split(rem, b).fold((rem, acc)) { case (rec1, rec2) => (rec1, rec2 :: acc) } else (rem, acc)
      }

    if (planarized.nonEmpty) planarized else List(record)
  }

  def domain(plot: P): Option[RangeP] = {
    val startO = Try(plot.records.map { case (range, _) => range.start }.min).toOption
    val endO = Try(plot.records.map { case (range, _) => range.end }.max).toOption

    for {
      start <- startO
      end <- endO
    } yield RangeP(start, end)
  }

  def add(wps: NonEmptyList[(Double, P)]): P = {
    val listWps = wps.toList
    val norm = listWps.map(_._1).sum
    val normWps = listWps.map {
      case (weight, plot) =>
        val normWeight = if (norm != 0) weight / norm else 1 / listWps.size.toDouble
        (normWeight, plot)
    }

    val concatRecords: List[Record] = normWps
      .map {
        case (weight, plot) =>
          plot.records.map { case (range, value) => (range, weight * value) }
      }
      .foldLeft(List.empty[Record]) { case (acc, rec) => acc ++ rec }

    val mgdRecords = planarizeRecords(concatRecords).map { case (range, values) => (range, values.sum) }

    unsafeModifyRecords(wps.head._2, _ => mgdRecords)
  }

  def concat(plot1: P, plot2: P): P =
    unsafeModifyRecords(
      plot1,
      records => {
        val sumList = records ++ plot2.records
        planarizeRecords(sumList).map {
          case (range, values) =>
            val max = values.max
            val min = values.min
            (range, if (math.abs(max) > math.abs(min)) max else min)
        }
      }
    )

  def multiplyConstant(plot: P, mag: Double): P = modifyValue(plot, { case (_, value) => value * mag })

  def integral(plot: P, start: Double, end: Double): Double = {
    lazy val startIndexedBlocksFromTo = plot.startIndexedBlocks.from(start).to(end)

    lazy val startBoundary: Double = (for {
      idxBlocks <- plot.startIndexedBlocks.to(start).lastOption
      (_, blocks) = idxBlocks
      ((x1, y1), (x2, y2)) = blocks
        .groupBy { case (_, (_x2, _)) => _x2 }
        .maxBy { case (_x2, _) => _x2 }
        ._2
        .maxBy { case ((_, _y1), (_, _y2)) => _y1 / 2 + _y2 / 2 }
      yi = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(start)
    } yield if (start != x1) areaPoint(start, yi, x2, y2) else 0).sum

    lazy val endBoundary: Double = {
      (for {
        idxBlocks <- plot.startIndexedBlocks.to(end).lastOption
        (_, blocks) = idxBlocks
        ((x1, y1), (x2, y2)) = blocks
          .groupBy { case (_, (_x2, _)) => _x2 }
          .maxBy { case (_x2, _) => _x2 }
          ._2
          .maxBy { case ((_, _y1), (_, _y2)) => _y1 / 2 + _y2 / 2 }
        yi = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil).interpolation(end)
      } yield areaPoint(x1, y1, end, yi)).sum
    }

    lazy val mid: Double = {
      (if (startIndexedBlocksFromTo.nonEmpty) {
         val endBlock = (for {
           idxBlocks <- startIndexedBlocksFromTo.lastOption
           (_, blocks) = idxBlocks
           block = blocks
             .groupBy { case (_, (_x2, _)) => _x2 }
             .maxBy { case (_x2, _) => _x2 }
             ._2
             .maxBy { case ((_, _y1), (_, _y2)) => _y1 / 2 + _y2 / 2 }
         } yield areaBlock(block))
           .getOrElse(0.0)

         Some(startIndexedBlocksFromTo.values.toList.map(blocks => areaBlocks(blocks)).sum - endBlock)
       } else None).sum
    }

    lazy val startEndBoundary: Double = {
      (if (startIndexedBlocksFromTo.isEmpty) {
         for {
           idxBlocks <- plot.startIndexedBlocks.to(end).lastOption
           (_, blocks) = idxBlocks
           block <- blocks.find {
             case ((x1, _), (x2, _)) => x1 <= start && x2 >= end
           }
           ((x1, y1), (x2, y2)) = block
           plot = CountPlot.disjoint((RangeP(x1), y1) :: (RangeP(x2), y2) :: Nil)
           yi1 = plot.interpolation(start)
           yi2 = plot.interpolation(end)
         } yield areaPoint(start, yi1, end, yi2)
       } else None).sum
    }

    if (startIndexedBlocksFromTo.nonEmpty) mid + startBoundary + endBoundary else startEndBoundary
  }

  def integralAll(plot: P): Double = domain(plot).map(range => integral(plot, range.start, range.end)).getOrElse(0.0)

  def areaPoint(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    if (y1 == 0 && y2 == 0) 0
    else RangeP(x1, x2).roughLength * (y2 / 2 + y1 / 2)
  }

  def areaBlock(block: Block): Double = block match {
    case ((x1, y1), (x2, y2)) => areaPoint(x1, y1, x2, y2)
  }

  def areaBlocks(blocks: List[Block]): Double = blocks.map(block => areaBlock(block)).sum

  def isEmpty(plot: P): Boolean = plot.records.isEmpty

  def cumulative(plot: P): P = {
    var accVal = 0d
    val cumHead = (RangeP.point(Double.MinValue), 0d)

    unsafeModifyRecords(
      plot,
      (records: List[Record]) => {
        val utdRecord = records.flatMap {
          case (range, value) =>
            val startVal = accVal
            val endVal = accVal + value * range.roughLength
            accVal = endVal

            (RangeP.point(range.start), startVal) :: (RangeP.point(range.end), endVal) :: Nil
        }

        utdRecord.headOption.fold(utdRecord)(utdHead =>
          if (utdHead._1 != cumHead._1) cumHead :: utdRecord else utdRecord)
      }
    )
  }

  def normalizedCumulative(plot: P): P = {
    val cum = cumulative(plot)
    val max = cum.records.lastOption.map(_._2).getOrElse(1.0)
    multiplyConstant(cum, 1 / max)
  }

  def inverseNormalizedCumulative(plot: P): P = {
    unsafeModifyRecords(
      normalizedCumulative(plot),
      (records: List[Record]) => records.map { case (range, value) => (RangeP.point(value), range.middle) })
  }

}

object RangePlot extends RangePlotOps[RangePlot] {

  // ops

  def modifyRecords(plot: RangePlot, f: List[Record] => List[Record]): RangePlot = plot match {
    case plot: DensityPlot => DensityPlot.modifyRecords(plot, f)
    case plot: CountPlot => CountPlot.modifyRecords(plot, f)
  }

  private[plot] def unsafeModifyRecords(plot: RangePlot, f: List[Record] => List[Record]): RangePlot = plot match {
    case plot: DensityPlot => DensityPlot.unsafeModifyRecords(plot, f)
    case plot: CountPlot => CountPlot.unsafeModifyRecords(plot, f)
  }

  def modifyValue(plot: RangePlot, f: Record => Double): RangePlot = plot match {
    case plot: DensityPlot => DensityPlot.modifyValue(plot, f)
    case plot: CountPlot => CountPlot.modifyValue(plot, f)
  }

  def split(record: Record, p: Double): Option[(Record, Record)] = DensityPlot.split(record, p)

}
