package sketch.scope.range

import sketch.scope.pdf.Prim

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  *
  * Range with primitive type of Sketch.
  */
trait RangeP {
  def start: Prim
  def end: Prim
  override def equals(other: Any): Boolean = other.isInstanceOf[RangeP] &&
    start == other.asInstanceOf[RangeP].start &&
    end == other.asInstanceOf[RangeP].end
  override def hashCode(): Int = start.hashCode() + end.hashCode()
}

trait RangePOps {

  def contains(range: RangeP, a: Prim): Boolean = {
    ((range.start >= a) && range.end <= a) ||
      ((range.start <= a) && (range.end >= a))
  }

  def middle(range: RangeP): Prim = (range.start - range.end) / 2

  def length(range: RangeP): Prim = range.end - range.start

}

trait RangePSyntax {

  implicit class RangeImpl(range: RangeP) {
    def contains(a: Prim): Boolean = RangeP.contains(range, a)
    def middle: Prim = RangeP.middle(range)
    def length: Prim = RangeP.length(range)
  }

}

object RangeP extends RangePOps {

  private case class RangePImpl(start: Prim, end: Prim) extends RangeP

  def apply(start: Prim, end: Prim): RangeP = bare(start, end)

  def bare(start: Prim, end: Prim): RangeP = RangePImpl(start, end)

  def point(p: Prim): RangeP = bare(p, p)

  def forNumericRange(numRange: NumericRange[Prim]): RangeP = apply(numRange.start, numRange.end)

}
