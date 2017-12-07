package sketch.scope.range

import sketch.scope.measure.Measure
import sketch.scope.pdf.Prim

/**
  * Licensed by Probe Technology, Inc.
  */
trait RangeM[A] {
  def measure: Measure[A]
  def start: A
  def end: A
  override def equals(other: Any): Boolean = other.isInstanceOf[RangeM[A]] && {
    val otherR = other.asInstanceOf[RangeM[A]]

    start == otherR.start &&
    measure.to(start) == otherR.measure.to(otherR.start)
    end == otherR.end &&
    measure.to(end) == otherR.measure.to(otherR.end)
  }
  override def hashCode(): Int = start.hashCode() + end.hashCode()
}

trait RangeMOps[R[_]<:RangeM[_]] {

  def primStart[A](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.start.asInstanceOf[A])

  def primEnd[A](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.end.asInstanceOf[A])

  def containsP(start: Prim, end: Prim, p: Prim): Boolean =
    ((start >= p) && end <= p) ||
      ((start <= p) && (end >= p))

  def contains[A](range: R[A], a: A): Boolean =
    containsP(primStart(range), primEnd(range), range.measure.asInstanceOf[Measure[A]].to(a))

  def middleP[A](start: Prim, end: Prim): Prim = (start + end) / 2

  def middle[A](range: R[A]): A =
    range.measure.asInstanceOf[Measure[A]].from(middleP(primStart(range), primEnd(range)))

}

trait RangeMSyntax {

  implicit class RangeMSyntaxImpl[A](range: RangeM[A]) {
    def contains(a: A): Boolean = RangeM.contains(range, a)
    def middle: A = RangeM.middle(range)
  }

}

object RangeM extends RangeMOps[RangeM]
