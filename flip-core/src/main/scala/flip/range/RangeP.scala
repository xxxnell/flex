package flip.range

import flip.measure._
import flip.measure.syntax._
import flip.pdf.Prim
import flip.range.syntax._

import scala.collection.immutable.NumericRange

/**
  * Range for primitive type of Sketch.
  */
trait GenericRangeP[G] extends RangeM[Prim] {
  def measure: Measure[Prim] = doubleMeasure
  def start: Prim
  def end: Prim
}

trait RangePOps extends RangeMOps[GenericRangeP] {

  def intersection[A](range1: RangeP, range2: RangeP): RangeP = {
    val (start1, end1) = (range1.start, range1.end)
    val (start2, end2) = (range2.start, range2.end)

    val start = if(start1 > start2) start1 else start2
    val end = if(end1 > end2) end2 else end1

    RangeP(start, end)
  }

  def overlapPercent[A](range1: RangeP, range2: RangeP): Double = {
    val range1Len = length(range1)

    if (range1Len != 0) (length(intersection(range1, range2)) / length(range1)).toDouble
    else if(range1 == range2) 1
    else 0
  }

  def modifyMeasure[A](range: RangeP, measure: Measure[A]): RangeM[A] =
    RangeM(measure.from(range.start), measure.from(range.end))(measure)

  def greater[A](range: RangeP, a: Prim): Boolean = range.start > a

  def less[A](range: RangeP, a: Prim): Boolean = range.end < a

  def contains[A](range: RangeP, a: Prim): Boolean = containsP(range.start, range.end, a)

  def geq(range: RangeP, a: Prim): Boolean = greater(range, a) || contains(range, a)

  def leq(range: RangeP, a: Prim): Boolean = less(range, a) || contains(range, a)

}

trait RangePSyntax {

  type RangeP = GenericRangeP[Nothing]

  implicit class RangeImpl(range: RangeP) {
    def length: BigDecimal = RangeP.length(range)
    def roughLength: Double = RangeP.roughLength(range)
    def overlapPercent(range2: RangeP): Double = RangeP.overlapPercent(range, range2)
    def modifyMeasure[A](measure: Measure[A]): RangeM[A] = RangeP.modifyMeasure(range, measure)
  }

}

object RangeP extends RangePOps {

  private case class RangePImpl(start: Prim, end: Prim) extends RangeP

  def apply(p: Prim): RangeP = point(p)

  def apply(start: Prim, end: Prim): RangeP = bare(start, end)

  def bare(start: Prim, end: Prim): RangeP = {
    if(start < end) RangePImpl(start, end) else RangePImpl(end, start)
  }

  def point(p: Prim): RangeP = bare(p, p)

  def forNumericRange(numRange: NumericRange[Prim]): RangeP = apply(numRange.start, numRange.end)

  def forRangeM[A](rangeM: RangeM[A]): RangeP = {
    bare(rangeM.measure.to(rangeM.start), rangeM.measure.to(rangeM.end))
  }

}
