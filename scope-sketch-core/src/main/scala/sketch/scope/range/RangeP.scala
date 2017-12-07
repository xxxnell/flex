package sketch.scope.range

import sketch.scope.measure._
import sketch.scope.pdf.Prim

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  *
  * Range with primitive type of Sketch.
  */
trait GenericRangeP[G] extends RangeM[Prim] {
  def measure: Measure[Prim] = doubleMeasure
  def start: Prim
  def end: Prim
}

trait RangePOps extends RangeMOps[GenericRangeP] {

  def length(range: RangeP): Prim = range.end - range.start

}

trait RangePSyntax {

  type RangeP = GenericRangeP[Nothing]

  implicit class RangeImpl(range: RangeP) {
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
