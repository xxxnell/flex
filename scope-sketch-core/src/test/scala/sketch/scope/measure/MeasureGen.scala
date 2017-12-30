package sketch.scope.measure

import org.scalacheck.Gen
import sketch.scope.measure.syntax._

/**
  * Licensed by Probe Technology, Inc.
  */
object MeasureGen {

  def intMeasureGen: Gen[Measure[Int]] = Gen.const(intMeasure)

  def doubleMeasureGen: Gen[Measure[Double]] = Gen.const(doubleMeasure)

  def booleanMeasureGen: Gen[Measure[Boolean]] = Gen.const(booleanMeasure)

}
