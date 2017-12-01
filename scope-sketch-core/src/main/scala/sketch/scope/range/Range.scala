package sketch.scope.range

import sketch.scope.pdf.Prim

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  */
trait Range {
  def start: Prim
  def end: Prim
  override def equals(other: Any): Boolean = other.isInstanceOf[Range] &&
    start == other.asInstanceOf[Range].start &&
    end == other.asInstanceOf[Range].end
  override def hashCode(): Int = start.hashCode() + end.hashCode()
}

trait RangeOps {

  def contains(range: Range, a: Prim): Boolean = {
    ((range.start >= a) && range.end <= a) ||
      ((range.start <= a) && (range.end >= a))
  }

  def middle(range: Range): Prim = (range.start - range.end) / 2

}

trait RangeSyntax {

  implicit class RangeImpl(range: Range) {
    def contains(a: Prim): Boolean = Range.contains(range, a)
    def middle: Prim = Range.middle(range)
  }

}

object Range extends RangeOps {

  private case class RangeImpl(start: Prim, end: Prim) extends Range

  def apply(start: Prim, end: Prim): Range = RangeImpl(start, end)

  def forNumericRange(numRange: NumericRange[Prim]): Range = apply(numRange.start, numRange.end)

}
