package sketch.scope.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import BenchOps._

/**
  * Licensed by Probe Technology, Inc.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class UnitBench {

  @Benchmark
  def updateUnit() = updateBench(defaultSketch, defaultSignals, 1)

  @Benchmark
  def rearrangeUnit() = rearrangeBench(defaultSketch)

}
