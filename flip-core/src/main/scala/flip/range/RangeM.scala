package flip.range

import flip.measure.Measure
import flip.pdf.Prim
import flip.range.syntax._

import scala.collection.immutable.NumericRange
import scala.language.{higherKinds, implicitConversions}

/**
  * Range for measurable value.
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
  override def toString: String = s"[$start..$end]"
}

trait RangeMOps[R[_]<:RangeM[_]] {

  def primStart[A](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.start.asInstanceOf[A])

  def primEnd[A](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.end.asInstanceOf[A])

  def containsP(start: Prim, end: Prim, p: Prim): Boolean = {
    ((start >= p) && (end <= p)) ||
      ((start <= p) && (end >= p))
  }

  def contains[A](range: R[A], a: A): Boolean = {
    containsP(primStart(range), primEnd(range), range.measure.asInstanceOf[Measure[A]].to(a))
  }

  def greater[A](range: R[A], a: A): Boolean = primStart(range) > range.measure.asInstanceOf[Measure[A]].to(a)

  def less[A](range: R[A], a: A): Boolean = primEnd(range) < range.measure.asInstanceOf[Measure[A]].to(a)

  def middleP[A](start: Prim, end: Prim): Prim = {
    if(start == Double.NegativeInfinity && end == Double.NegativeInfinity) Double.NegativeInfinity
    else if(start == Double.PositiveInfinity && end == Double.PositiveInfinity) Double.PositiveInfinity
    else ((BigDecimal(start) + BigDecimal(end)) / 2).toDouble
  }

  def middle[A](range: R[A]): A =
    range.measure.asInstanceOf[Measure[A]].from(middleP(primStart(range), primEnd(range)))

  def isForward(range: R[_]): Boolean = if(primEnd(range) - primStart(range) >= 0) true else false

  def isPoint(range: R[_]): Boolean = if(range.start == range.end) true else false

  def length[A](range: R[A]): BigDecimal = BigDecimal(primEnd(range)) - BigDecimal(primStart(range))

}

trait RangeMSyntax {

  implicit def scalaRange2RangeMs(range: Range): List[RangeM[Int]] =
    scalaNumericRange2RangeMs(NumericRange(range.start, range.end, range.step))

  implicit def scalaNumericRange2RangeMs[A](range: NumericRange[A])
                                           (implicit measure: Measure[A]): List[RangeM[A]] =
    range.toList
      .sliding(2).toList
      .flatMap {
        case a1 :: a2 :: Nil => Some((a1, a2))
        case _ => None
      }.map { case (start, end) => RangeM(start, end) }

  implicit class RangeMSyntaxImpl[A](range: RangeM[A]) {
    def contains(a: A): Boolean = RangeM.contains(range, a)
    def greater(a: A): Boolean = RangeM.greater(range, a)
    def >(a: A): Boolean = RangeM.greater(range, a)
    def >=(a: A): Boolean = RangeM.greater(range, a) || RangeM.contains(range, a)
    def less(a: A): Boolean = RangeM.less(range, a)
    def <(a: A): Boolean = RangeM.less(range, a)
    def <=(a: A): Boolean = RangeM.less(range, a) || RangeM.contains(range, a)
    def middle: A = RangeM.middle(range)
    def isPoint: Boolean = RangeM.isPoint(range)
    def length: BigDecimal = RangeM.length(range)
  }

}

object RangeM extends RangeMOps[RangeM] {

  case class RangeMImpl[A](start: A, end: A, measure: Measure[A]) extends RangeM[A]

  def apply[A](start: A, end: A)(implicit measure: Measure[A]): RangeM[A] = bare(start, end, measure)

  def bare[A](start: A, end: A, measure: Measure[A]): RangeM[A] = {
    if(measure(start) < measure(end)) RangeMImpl(start, end, measure) else RangeMImpl(end, start, measure)
  }

  override def greater[A](range: RangeM[A], a: A): Boolean = (range, a) match {
    case (range: RangeP, a: Prim) => RangeP.greater(range, a)
    case _ => super.greater(range, a)
  }

  override def less[A](range: RangeM[A], a: A): Boolean = (range, a) match {
    case (range: RangeP, a: Prim) => RangeP.less(range, a)
    case _ => super.less(range, a)
  }

  override def contains[A](range: RangeM[A], a: A): Boolean = (range, a) match {
    case (range: RangeP, a: Prim) => RangeP.contains(range, a)
    case _ => super.contains(range, a)
  }

}
