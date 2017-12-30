package sketch.scope.benchmark

import sketch.scope._

/**
  * Licensed by Probe Technology, Inc.
  */
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

  def rearrangeBench[A](sketch: Sketch[A]): Option[Sketch[A]] = sketch.rearrange

}
