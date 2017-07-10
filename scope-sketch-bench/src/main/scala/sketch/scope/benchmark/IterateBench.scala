package sketch.scope.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import BenchOps._

/**
  * Licensed by Probe Technology, Inc.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class IterateBench {

  @Param(Array("50", "100", "150", "200"))
  var size: Int = _

  var xs: Array[Int] = _

  @Setup
  def setup() {
    xs = Array.ofDim[Int](size)
    for (c <- 0 until size) {
      xs(c) = c + 1
    }
  }

  @Benchmark
  def iterate(bh: Blackhole) {
    for (x <- xs) {
      bh.consume(updateBench(defaultSketch, defaultSignals, x))
    }
  }

}
