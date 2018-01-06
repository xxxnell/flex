package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip.benchmark.ops.SketchBenchOps._
import flip.pdf.Sketch
import flip.measure._
import flip.conf._

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
