package flex.benchmark

import java.util.concurrent.TimeUnit

import flex._
import flex.benchmark.ops.SignalOps
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class IterateBench { self =>

  // params

  @Param(Array("30"))
  var bufferSizeL: Int = _

  @Param(Array("0"))
  var iterateBenchSize: Int = _

  @Param(Array("3"))
  var cmapNoL: Int = _

  @Param(Array("20"))
  var cmapSizeL: Int = _

  @Param(Array("0.2"))
  var rebuildThresholdL: Double = _

  // variables

  var signals: Array[Double] = _

  var sketch: Sketch[Double] = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      bufferSize = bufferSizeL,
      cmapSize = cmapSizeL,
      cmapNo = cmapNoL,
      cmapStart = Some(-3.0),
      cmapEnd = Some(3.0),
      rebuildThreshold = rebuildThresholdL)

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
