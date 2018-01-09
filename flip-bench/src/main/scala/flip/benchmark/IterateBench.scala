package flip.benchmark

import java.util.concurrent.TimeUnit

import flip._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import flip.benchmark.ops.SketchBenchOps._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class IterateBench {

  @Param(Array("50", "100", "150", "200"))
  var iterateBenchSize: Int = _

  @Param(Array("5"))
  var cmapNo: Int = _

  @Param(Array("200"))
  var cmapSize: Int = _

  @Param(Array("2"))
  var counterNo: Int = _

  @Param(Array("70"))
  var counterSize: Int = _

  var sketch: Sketch[Double] = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 100, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    sketch = Sketch.empty[Double]
  }

  @Benchmark
  def iterate(bh: Blackhole) {
    bh.consume(updateBench(sketch, defaultSignals, iterateBenchSize))
  }

}
