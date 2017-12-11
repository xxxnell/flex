package sketch.scope.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import SketchBenchOps._
import sketch.scope.pdf.Sketch
import sketch.scope.measure._
import sketch.scope.conf._

/**
  * Licensed by Probe Technology, Inc.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SketchOpsBench {

  @Param(Array("5", "20"))
  var caDepth: Int = _

  @Param(Array("5000", "20000"))
  var caSize: Int = _

  @Param(Array("1", "30"))
  var coDepth: Int = _

  @Param(Array("1000", "100000"))
  var coSize: Int = _

  val sketch = Sketch.empty[Double]

  @Benchmark
  def construct = {
    Sketch.empty[Double]
  }

  @Benchmark
  def update = {
    updateBench(sketch, defaultSignals, 1)
  }

  @Benchmark
  def rearrange = {
    rearrangeBench(sketch)
  }

}
