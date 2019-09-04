package flex.range

import flex.measure.Measure
import flex.pdf.Prim
import flex.range.syntax._
import flex.measure.syntax._

import scala.collection.immutable.NumericRange
import scala.language.{ higherKinds, implicitConversions, reflectiveCalls }

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

trait RangeMOps[Γ, R[_] <: RangeM[_]] {

  def modifyRange[A <: Γ](range: R[A], f: (A, A) => (A, A)): R[A]

  def modifyMeasure[A <: Γ, B](range: R[A], measure: Measure[B]): R[B]

  def setRange[A <: Γ](range: R[A], start: A, end: A): R[A] =
    modifyRange(range, (_: A, _: A) => (start, end))

  def startP[A <: Γ](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.start.asInstanceOf[A])

  def endP[A <: Γ](range: R[A]): Prim = range.measure.asInstanceOf[Measure[A]].to(range.end.asInstanceOf[A])

  def rangeP[A <: Γ](range: R[A]): (Prim, Prim) = (startP(range), endP(range))

  def containsP(start: Prim, end: Prim, p: Prim): Boolean =
    ((start >= p) && (end <= p)) ||
    ((start <= p) && (end >= p))

  def contains[A <: Γ](range: R[A], a: A): Boolean =
    containsP(startP(range), endP(range), range.measure.asInstanceOf[Measure[A]].to(a))

  def greater[A <: Γ](range: R[A], a: A): Boolean = startP(range) > range.measure.asInstanceOf[Measure[A]].to(a)

  def less[A <: Γ](range: R[A], a: A): Boolean = endP(range) < range.measure.asInstanceOf[Measure[A]].to(a)

  def divMiddleP(start: Prim, end: Prim): Prim =
    if (start.isNegInfinity && end.isNegInfinity) Double.NegativeInfinity
    else if (start.isPosInfinity && end.isPosInfinity) Double.PositiveInfinity
    else if (start.isNegInfinity && end.isPosInfinity) Double.NaN
    else start + (end / 2 - start / 2)

  def cutoffMiddleP(start: Prim, end: Prim): Prim =
    if (start.isNegInfinity && end.isNegInfinity) Double.NegativeInfinity
    else if (start.isPosInfinity && end.isPosInfinity) Double.PositiveInfinity
    else if (start.isNegInfinity && end.isPosInfinity) Double.NaN
    else start.cutoff + (end.cutoff / 2 - start.cutoff / 2)

  def divMiddle[A <: Γ](range: R[A]): A =
    range.measure.asInstanceOf[Measure[A]].from(divMiddleP(startP(range), endP(range)))

  def cutoffMiddle[A <: Γ](range: R[A]): A =
    range.measure.asInstanceOf[Measure[A]].from(cutoffMiddleP(startP(range), endP(range)))

  def isForward[A <: Γ](range: R[A]): Boolean = if (endP(range) - startP(range) >= 0) true else false

  def isPoint(range: R[_]): Boolean = if (range.start == range.end) true else false

  def length[A <: Γ](range: R[A]): BigDecimal =
    BigDecimal(endP(range)) - BigDecimal(startP(range))

  def divLength[A <: Γ](range: R[A]): Double =
    endP(range) - startP(range)

  def cutoffLength[A <: Γ](range: R[A]): Double = {
    val roughLength = endP(range) - startP(range)
    if (roughLength.isPosInfinity) Double.MaxValue
    else if (roughLength.isNegInfinity) Double.MinValue
    else roughLength
  }

  def uniformSplit[A <: Γ](range: R[A], size: Int): List[R[A]] = {
    val measure = range.measure.asInstanceOf[Measure[A]]
    val (startP, endP) = rangeP(range)
    val unit = (endP - startP) / size
    lazy val unitB = (BigDecimal(endP) - BigDecimal(startP)) / size

    (0 until size).toList
      .map(i => if (!unit.isNaN && !unit.isInfinity) startP + i * unit else (startP + i * unitB).toDouble)
      .sliding(2)
      .toList
      .flatMap {
        case p1 :: p2 :: Nil => Some(setRange(range, measure.from(p1), measure.from(p2)))
        case _ => None
      }
  }

}

object RangeM extends RangeMOps[Any, RangeM] {

  case class RangeMImpl[A](start: A, end: A, measure: Measure[A]) extends RangeM[A]

  def apply[A](start: A, end: A)(implicit measure: Measure[A]): RangeM[A] = bare(start, end, measure)

  def bare[A](start: A, end: A, measure: Measure[A]): RangeM[A] =
    if (measure(start) < measure(end)) RangeMImpl(start, end, measure) else RangeMImpl(end, start, measure)

  def modifyRange[A](range: RangeM[A], f: (A, A) => (A, A)): RangeM[A] = {
    val (start, end) = f(range.start, range.end)
    bare(start, end, range.measure)
  }

  def modifyMeasure[A, B](range: RangeM[A], measure: Measure[B]): RangeM[B] = {
    val (startP, endP) = rangeP(range)
    bare(measure.from(startP), measure.from(endP), measure)
  }

}
