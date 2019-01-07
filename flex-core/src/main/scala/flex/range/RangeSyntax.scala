package flex.range

import flex.measure.Measure
import flex.measure.syntax.doubleMeasure
import flex.pdf.Prim

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

trait RangeSyntax {

  type RangeP = RangeM[Prim]

  implicit def scalaRange2RangeMs(range: Range): List[RangeM[Int]] =
    scalaNumericRange2RangeMs(NumericRange(range.start, range.end, range.step))

  implicit def scalaNumericRange2RangeMs[A](range: NumericRange[A])(implicit measure: Measure[A]): List[RangeM[A]] =
    range.toList
      .sliding(2)
      .toList
      .flatMap {
        case a1 :: a2 :: Nil => Some((a1, a2))
        case _ => None
      }
      .map { case (start, end) => RangeM(start, end) }

  implicit class RangeMSyntaxImpl[A](range: RangeM[A]) {
    def modifyMeasure[B](measure: Measure[B]): RangeM[B] = RangeM.modifyMeasure(range, measure)
    def primitivize: RangeP = RangeM.modifyMeasure(range, doubleMeasure)
    def contains(a: A): Boolean = RangeM.contains(range, a)
    def greater(a: A): Boolean = RangeM.greater(range, a)
    def >(a: A): Boolean = RangeM.greater(range, a)
    def >=(a: A): Boolean = RangeM.greater(range, a) || RangeM.contains(range, a)
    def less(a: A): Boolean = RangeM.less(range, a)
    def <(a: A): Boolean = RangeM.less(range, a)
    def <=(a: A): Boolean = RangeM.less(range, a) || RangeM.contains(range, a)
    def middle: A = RangeM.divMiddle(range)
    def divMiddle: A = RangeM.divMiddle(range)
    def cutoffMiddle: A = RangeM.cutoffMiddle(range)
    def isPoint: Boolean = RangeM.isPoint(range)
    def length: BigDecimal = RangeM.length(range)
    def roughLength: Double = RangeM.cutoffLength(range)
    def cutoffLength: Double = RangeM.cutoffLength(range)
    def divLength: Double = RangeM.divLength(range)
    def uniformSplit(size: Int): List[RangeM[A]] = RangeM.uniformSplit(range, size)
  }

  implicit class RangeImpl(range: RangeP) {
    def overlapPercent(range2: RangeP): Double = RangeP.overlapPercent(range, range2)
  }

}
