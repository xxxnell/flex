package sketch.scope.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import SketchBenchOps._

/**
  * Licensed by Probe Technology, Inc.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class IterateBench {

  @Param(Array("50", "100", "150", "200"))
  var iterateBenchSize: Int = _

  @Benchmark
  def iterate(bh: Blackhole) {
    bh.consume(updateBench(defaultSketch, defaultSignals, iterateBenchSize))
  }

}
