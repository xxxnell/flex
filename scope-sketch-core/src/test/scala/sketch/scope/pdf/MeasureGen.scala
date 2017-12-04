package sketch.scope.pdf

import org.scalacheck.Gen
import sketch.scope.measure._

/**
  * Licensed by Probe Technology, Inc.
  */
object MeasureGen {

  def intMeasureGen: Gen[Measure[Int]] = Gen.const(intMeasure)

  def doubleMeasureGen: Gen[Measure[Double]] = Gen.const(doubleMeasure)

  def booleanMeasureGen: Gen[Measure[Boolean]] = Gen.const(booleanMeasure)

}
