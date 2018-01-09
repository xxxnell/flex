package flip.benchmark.ops

import flip._

object SketchBenchOps {

  val defaultSketch: Sketch[Double] = Sketch.empty[Double]

  val defaultSignals: Stream[Double] = Stream.iterate(0.0)(_ + 0.1)

  def updateBench(sketch: Sketch[Double], signals: Stream[Double], n: Int): Option[Sketch[Double]] = {
    signals
      .take(n)
      .foldLeft(Option(sketch)){ case (sketchO, signal) =>
        sketchO.flatMap(sketch => sketch.update(signal))
      }
  }

}
