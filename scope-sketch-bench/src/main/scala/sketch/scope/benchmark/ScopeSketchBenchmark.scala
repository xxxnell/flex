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

  def updateBench(sketch: Sketch, signals: Stream[Double], n: Int): Option[Sketch] = {
    signals
      .take(n)
      .foldLeft(Option(sketch)){ case (sketchO, signal) => sketchO.flatMap(sketch => sketch.update(signal)) }
  }

  def rearrangeBench(sketch: Sketch): Option[Sketch] = sketch.rearrange

  @Benchmark
  def updateBenchUnit() = updateBench(sketch, signals, 1)

  @Benchmark
  def updateBench1K() = updateBench(sketch, signals, 1000)

  @Benchmark
  def rearrangeBenchUnit() = rearrangeBench(sketch)

}
