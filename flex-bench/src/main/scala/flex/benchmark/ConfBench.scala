package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.benchmark.ops.SignalOps
import flex.implicits._
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

  var normalSignals: Array[Double] = _

  var incrDriftSignals: Array[Double] = _

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
      rebuildThreshold = rebuildThresholdLS)

    normalSignals = SignalOps.normalSignals(iterateBenchSizeLS).toArray
    incrDriftSignals = SignalOps.incrDriftSignals(iterateBenchSizeLS, 0.01).toArray
    sketch = Sketch.empty[Double]
  }

  @Benchmark
  def normal(bh: Blackhole): Unit = bh.consume {
    var i = 0
    var sketch: Sketch[Double] = self.sketch
    while (i < normalSignals.length) {
      sketch = sketch.update(normalSignals(i))
      i += 1
    }
  }

  @Benchmark
  def incrDrift(bh: Blackhole): Unit = bh.consume {
    var i = 0
    var sketch: Sketch[Double] = self.sketch
    while (i < incrDriftSignals.length) {
      sketch = sketch.update(incrDriftSignals(i))
      i += 1
    }
  }

}
