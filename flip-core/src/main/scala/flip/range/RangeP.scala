package flip.range

import flip.measure._
import flip.measure.syntax._
import flip.pdf.Prim
import flip.range.syntax._

import scala.collection.immutable.NumericRange
import scala.language.higherKinds

/**
  * Range for primitive type of Sketch.
  */
trait RangePOps[R[_] <: RangeM[_]] extends RangeMOps[Prim, R] {

  def intersection[A <: Prim](range1: R[A], range2: R[A]): R[A] = {
    val (start1, end1) = rangeP(range1)
    val (start2, end2) = rangeP(range2)
    val measure = range1.measure.asInstanceOf[Measure[A]]

    val startP = if (start1 > start2) start1 else start2
    val endP = if (end1 > end2) end2 else end1

    setRange(range1, measure.from(startP), measure.from(endP))
  }

  def overlapPercent[A <: Prim](range1: R[A], range2: R[A]): Double = {
    val (range1Start, range1End) = rangeP(range1)
    val inters = intersection(range1, range2)
    val (intersStart, intersEnd) = rangeP(inters)

    if (isPoint(inters)) 0
    else if (range1 == inters) 1
    else if (ceilLength(range1) != 0) {
      lazy val perc1 = (intersEnd - intersStart) / (range1End - range1Start)
      lazy val perc2 = (intersEnd / range1End) * (1 - intersStart / intersEnd) / (1 - range1Start / range1End)
      lazy val perc3 = (length(intersection(range1, range2)) / length(range1)).toDouble

      if (!perc1.isNaN && !perc1.isInfinity) perc1
      else if (!perc2.isNaN && perc2.isInfinity) perc2
      else perc3
    } else 0
  }

}

object RangeP extends RangePOps[RangeM] {

  private case class RangePImpl(start: Prim, end: Prim) extends RangeM[Prim] {
    val measure: Measure[Prim] = doubleMeasure
  }

  def apply(p: Prim): RangeP = point(p)

  def apply(start: Prim, end: Prim): RangeP = bare(start, end)

  def bare(start: Prim, end: Prim): RangeP = {
    if (start < end) RangePImpl(start, end) else RangePImpl(end, start)
  }

  def point(p: Prim): RangeP = bare(p, p)

  def forNumericRange(numRange: NumericRange[Prim]): RangeP = apply(numRange.start, numRange.end)

  def forRangeM[A](rangeM: RangeM[A]): RangeP = {
    bare(rangeM.measure.to(rangeM.start), rangeM.measure.to(rangeM.end))
  }

  def modifyRange[A <: Prim](range: RangeM[A], f: (A, A) => (A, A)): RangeM[A] =
    RangeM.modifyRange(range, f)

  def modifyMeasure[A <: Prim, B](range: RangeM[A], measure: Measure[B]): RangeM[B] =
    RangeM.modifyMeasure(range, measure)

}
