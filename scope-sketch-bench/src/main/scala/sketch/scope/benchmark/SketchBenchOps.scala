package sketch.scope.benchmark

import sketch.scope.sketch.Sketch

/**
  * Licensed by Probe Technology, Inc.
  */
object SketchBenchOps {

  val defaultSketch = Sketch.empty(5, 1000, 1, 2000)

  val defaultSignals: Stream[Double] = Stream.iterate(0.0)(_ + 0.1)

  def updateBench(sketch: Sketch, signals: Stream[Double], n: Int): Option[Sketch] = {
    signals
      .take(n)
      .foldLeft(Option(sketch)){ case (sketchO, signal) =>
        sketchO.flatMap(sketch => sketch.update(signal))
      }
  }

  def rearrangeBench(sketch: Sketch): Option[Sketch] = sketch.rearrange

}
