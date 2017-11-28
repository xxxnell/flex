package sketch.scope.pdf

import org.scalacheck.Gen

/**
  * Licensed by Probe Technology, Inc.
  */
object MeasureGen {

  def intMeasure: Int => Double = i => i.toDouble

  def intMeasureGen: Gen[Int => Double] = Gen.const(intMeasure)

  def doubleMeasure: Double => Double = d => d

  def doubleMeasureGen: Gen[Double => Double] = Gen.const(d => d)

  def booleanMeasure: Boolean => Double = b => if(b) 1d else 0d

}
