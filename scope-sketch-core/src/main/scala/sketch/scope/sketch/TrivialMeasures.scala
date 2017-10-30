package sketch.scope.sketch

/**
  * Licensed by Probe Technology, Inc.
  */
trait TrivialMeasures {

//  implicit val doubleMeasure: Double => Prim = (x: Double) => x

  implicit val intMeasure: Int => Prim = (x: Int) => x.toDouble

}
