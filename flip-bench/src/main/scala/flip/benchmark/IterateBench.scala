package flip.benchmark

import java.util.concurrent.TimeUnit

import flip._
import flip.benchmark.ops.SignalOps
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class IterateBench { self =>

  // params

  @Param(Array("30"))
  var bufferSize: Int = _

  @Param(Array("0"))
  var iterateBenchSize: Int = _

  @Param(Array("3"))
  var cmapNo: Int = _

  @Param(Array("20"))
  var cmapSize: Int = _

  // variables

  var signals: Array[Double] = _

  var sketch: Sketch[Double] = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 100,
      thresholdPeriod = 100,
      bufferSize = bufferSize,
      cmapSize = cmapSize,
      cmapNo = cmapNo,
      cmapStart = Some(-10d),
      cmapEnd = Some(10d)
    )

    signals = SignalOps.normalSignals(iterateBenchSize).toArray
    sketch = Sketch.empty[Double]
  }

  @Benchmark
  def iterate(bh: Blackhole): Unit = bh.consume {
    var i = 0
    var sketch: Sketch[Double] = self.sketch
    while (i < signals.length) {
      sketch = sketch.update(signals(i))
      i += 1
    }
  }

}
