package flex.measure

trait TrivialMeasures {

  implicit val intMeasure: Measure[Int] = Measure((x: Int) => x.toDouble, (y: Double) => Math.round(y).toInt)

  implicit val longMeasure: Measure[Long] = Measure((x: Long) => x.toDouble, (y: Double) => Math.round(y))

  implicit val doubleMeasure: Measure[Double] = Measure(identity, identity)

  implicit val floatMeasure: Measure[Float] = Measure((x: Float) => x.toDouble, (y: Double) => y.toFloat)

  implicit val booleanMeasure: Measure[Boolean] = Measure(
    (x: Boolean) => if (x) 1 else 0,
    (y: Double) => if (y > 0.5) true else false
  )

}
