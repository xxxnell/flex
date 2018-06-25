package flip.benchmark

import java.util.concurrent.TimeUnit

import flip.{Sketch, SketchConf}
import flip.benchmark.ops.SignalOps
import flip.implicits._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

/**
  * Large scale benchmark for Update of Sketch
  * */
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
class ConfBench { self =>

  // parameters

  @Param(Array("1000"))
  var iterateBenchSizeLS: Int = _

  @Param(Array("20"))
  var cmapSizeLS: Int = _

  @Param(Array("3"))
  var cmapNoLS: Int = _

  @Param(Array("50"))
  var bufferSizeLS: Int = _

  @Param(Array("1.0"))
  var decayFactorLS: Double = _

  @Param(Array("0.2"))
  var rebuildThresholdLS: Double = _

  // variables

  var signals: Array[Double] = _

  var sketch: Sketch[Double] = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      cmapSize = cmapSizeLS,
      cmapNo = cmapNoLS,
      cmapStart = Some(-20),
      cmapEnd = Some(20),
      bufferSize = bufferSizeLS,
      thresholdPeriod = bufferSizeLS,
      decayFactor = decayFactorLS,
      rebuildThreshold = rebuildThresholdLS
    )

    signals = SignalOps.normalSignals(iterateBenchSizeLS).toArray
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
