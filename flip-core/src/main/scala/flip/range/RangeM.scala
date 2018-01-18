package flip.range

import flip.measure.Measure
import flip.pdf.Prim
import flip.range.syntax._

import scala.collection.immutable.NumericRange
import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

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
    measure.to(start) == otherR.measure.to(otherR.start) &&
    end == otherR.end &&
    measure.to(end) == otherR.measure.to(otherR.end)
  }
  override def hashCode(): Int = start.hashCode() + end.hashCode()
  override def toString: String = s"[$start..$end]"
}

trait RangeMOps[R[_]<:RangeM[_]] {

  def modifyRange[A](range: R[A], f: (A, A) => (A, A)): R[A]

  def modifyMeasure[A, B](range: R[A], measure: Measure[B]): R[B]

  def startP[A](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.start.asInstanceOf[A])

  def endP[A](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.end.asInstanceOf[A])

  def rangeP[A](range: R[A]): (Prim, Prim) = (startP(range), endP(range))

  def containsP(start: Prim, end: Prim, p: Prim): Boolean = {
    ((start >= p) && (end <= p)) ||
      ((start <= p) && (end >= p))
  }

  def contains[A](range: R[A], a: A): Boolean = {
    containsP(startP(range), endP(range), range.measure.asInstanceOf[Measure[A]].to(a))
  }

  def greater[A](range: R[A], a: A): Boolean = startP(range) > range.measure.asInstanceOf[Measure[A]].to(a)

  def less[A](range: R[A], a: A): Boolean = endP(range) < range.measure.asInstanceOf[Measure[A]].to(a)

  def middleP[A](start: Prim, end: Prim): Prim = {
    if(start == Double.NegativeInfinity && end == Double.NegativeInfinity) Double.NegativeInfinity
    else if(start == Double.PositiveInfinity && end == Double.PositiveInfinity) Double.PositiveInfinity
    else start + ((end - start) / 2)
  }

  def middle[A](range: R[A]): A =
    range.measure.asInstanceOf[Measure[A]].from(middleP(startP(range), endP(range)))

  def isForward[A](range: R[A]): Boolean = if(endP(range) - startP(range) >= 0) true else false

  def isPoint(range: R[_]): Boolean = if(range.start == range.end) true else false

  def length[A](range: R[A]): BigDecimal = {
    BigDecimal(endP(range)) - BigDecimal(startP(range))
  }

  def roughLength[A](range: R[A]): Double = {
    val roughLength = endP(range) - startP(range)
    if(roughLength.isPosInfinity) Double.MaxValue
    else if(roughLength.isNegInfinity) Double.MinValue
    else roughLength
  }

}

object RangeM extends RangeMOps[RangeM] {

  case class RangeMImpl[A](start: A, end: A, measure: Measure[A]) extends RangeM[A]

  def apply[A](start: A, end: A)(implicit measure: Measure[A]): RangeM[A] = bare(start, end, measure)

  def bare[A](start: A, end: A, measure: Measure[A]): RangeM[A] = {
    if(measure(start) < measure(end)) RangeMImpl(start, end, measure) else RangeMImpl(end, start, measure)
  }

  def modifyRange[A](range: RangeM[A], f: (A, A) => (A, A)): RangeM[A] = {
    val (start, end) = f(range.start, range.end)
    bare(start, end, range.measure)
  }

  def modifyMeasure[A, B](range: RangeM[A], measure: Measure[B]): RangeM[B] = {
    val (startP, endP) = rangeP(range)
    bare(measure.from(startP), measure.from(endP), measure)
  }

}
