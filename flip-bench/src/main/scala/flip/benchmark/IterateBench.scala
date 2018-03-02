package flip.benchmark

import java.util.concurrent.TimeUnit

import flip._
import flip.benchmark.ops.SignalOps
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class IterateBench {

  // params

  @Param(Array("30"))
  var queueSize: Int = _

  @Param(Array("50", "100", "150", "200"))
  var iterateBenchSize: Int = _

  @Param(Array("2"))
  var cmapNo: Int = _

  @Param(Array("20"))
  var cmapSize: Int = _

  @Param(Array("2"))
  var counterNo: Int = _

  @Param(Array("100000"))
  var counterSize: Int = _

  // variables

  var signals: List[Double] = _

  var sketch: Sketch[Double] = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 100,
      thresholdPeriod = 100,
      queueSize = queueSize,
      cmapSize = cmapSize,
      cmapNo = cmapNo,
      cmapStart = Some(-10d),
      cmapEnd = Some(10d),
      counterSize = counterSize,
      counterNo = counterNo
    )

    signals = SignalOps.normalSignals(iterateBenchSize)
    sketch = Sketch.empty[Double]
  }

  @Benchmark
  def iterate(bh: Blackhole): Unit = bh.consume {
    val n = iterateBenchSize

    var i = 0
    var sketchO: Option[Sketch[Double]] = Some(sketch)
    while (i < n) {
      sketchO = sketchO.flatMap(sketch => sketch.update(signals(i)))
      i += 1
    }
  }

}
