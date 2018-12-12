package flex.experiment

object GettingStartedExp {

  def main(args: Array[String]): Unit = {
    import flex.implicits._

    // get 100 random variables from standard normal distribution
    val underlying = NumericDist.normal(0.0, 1.0)
    val (_, samples) = underlying.samples(100)

    // update samples to sketch
    val sketch0 = Sketch.empty[Double]
    val sketch1 = samples.foldLeft(sketch0) {
      case (sketch, sample) => sketch.update(sample)
    }

    // analyze sketch
    println(
      s"Estimated Pr(0.0 ≤ x ≤ 1.0): ${sketch1.probability(0.0, 1.0)}, " +
        s"Expected Pr(0.0 ≤ x ≤ 1.0): ${underlying.probability(0.0, 1.0)}\n" +
        s"Estimated median: ${sketch1.median}, expected median: 0.0 \n" +
        s"Sample from sketch: ${sketch1.sample._2} \n" +
        s"KL-divergence: ${KLD(underlying, sketch1)}"
    )
  }

}
