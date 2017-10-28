package sketch.scope.range

import sketch.scope.sketch.Prim

/**
  * Licensed by Probe Technology, Inc.
  */
trait Range {
  def start: Prim
  def end: Prim
}

trait RangeOps {

  def contains(range: Range, a: Prim): Boolean = {
    ((range.start >= a) && range.end <= a) ||
      ((range.start <= a) && (range.end >= a))
  }

}

trait RangeSyntax {

  implicit class RangeImpl(range: Range) {
    def contains(a: Prim): Boolean = Range.contains(range, a)
  }

}

object Range extends RangeOps {

  private case class RangeImpl(start: Prim, end: Prim) extends Range

  def apply(start: Prim, end: Prim): Range = RangeImpl(start, end)

}
