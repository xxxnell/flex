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

  def intersection[A](range1: RangePA, range2: RangePA): RangeP = {
    val (start1, end1) = (range1.start, range1.end)
    val (start2, end2) = (range2.start, range2.end)

    val start = if(start1 > start2) start1 else start2
    val end = if(end1 > end2) end2 else end1

    RangeP(start, end)
  }

  def overlapPercent[A](range1: RangePA, range2: RangePA): Double = {
    val inters = intersection(range1, range2)

    if(roughLength(inters) == 0) 0
    else if(range1 == inters) 1
    else if(roughLength(range1) != 0) {
      lazy val perc1 = (inters.end - inters.start) / (range1.end - range1.start)
      lazy val perc2 = (inters.end / range1.end) * (1 - inters.start / inters.end) / (1 - range1.start / range1.end)
      lazy val perc3 = (length(intersection(range1, range2)) / length(range1)).toDouble

      if(!perc1.isNaN && !perc1.isInfinity) perc1
      else if(!perc2.isNaN && perc2.isInfinity) perc2
      else perc3
    } else 0
  }

  def modifyMeasure[A](range: RangePA, measure: Measure[A]): RangeM[A] =
    RangeM(measure.from(range.start), measure.from(range.end))(measure)

  def greater[A](range: RangePA, a: Prim): Boolean = range.start > a

  def less[A](range: RangePA, a: Prim): Boolean = range.end < a

  def contains[A](range: RangePA, a: Prim): Boolean = containsP(range.start, range.end, a)

  def geq(range: RangePA, a: Prim): Boolean = greater(range, a) || contains(range, a)

  def leq(range: RangePA, a: Prim): Boolean = less(range, a) || contains(range, a)

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
