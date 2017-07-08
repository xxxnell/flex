package sketch.scope.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import sketch.scope.sketch.Sketch

/**
  * Licensed by Probe Technology, Inc.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class ScopeSketchBenchmark {

  val sketch = Sketch.empty(10, 1000, 20, 2000)

  val signals: Stream[Double] = Stream.iterate(0.0)(_ + 0.1)

  @Benchmark
  def updatePerformance(): Unit = {
    signals
      .take(1000)
      .foldLeft(Option(sketch)){ case (sketchO, signal) => sketchO.flatMap(sketch => sketch.update(signal)) }
  }

}
