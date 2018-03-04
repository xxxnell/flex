package flip.experiment

object GettingStartedExp {

  def main(args: Array[String]): Unit = {
    import flip.implicits._

    // get 100 random variables from standard normal distribution
    val underlying = NumericDist.normal(0.0, 1.0)
    val (_, samples) = underlying.samples(100)

    // update samples to sketch
    val sketch0 = Sketch.empty[Double]
    val utdSketch = samples.foldLeft(sketch0) {
      case (sketch, sample) => sketch.update(sample)
    }

    // get probability for interval [0.0, 1.0]
    println("result: " + utdSketch.probability(0.0, 1.0))
    println("expected: " + underlying.probability(0.0, 1.0))
    // result: 0.35479611850109305
    // expected: 0.34134474606854304
  }

}
