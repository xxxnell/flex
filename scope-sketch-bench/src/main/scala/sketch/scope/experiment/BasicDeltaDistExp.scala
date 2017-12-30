package sketch.scope.experiment

import sketch.scope.{ExpOutOps, _}

/**
  * Licensed by Probe Technology, Inc.
  */
object BasicDeltaDistExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-delta"
    val sampleNo = 10

    implicit val conf: SketchConf = SketchConf(
      startThreshold = 2, thresholdPeriod = 1,
      cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = 1000, counterNo = 2
    )
    val sketch = Sketch.empty[Double]
    val (_, datas) = Dist.delta(0.1).samples(sampleNo)
    val dataIdxs = datas.zipWithIndex

    var tempSketchO: Option[Sketch[Double]] = Option(sketch)
    val idxUtdSketches: List[(Int, Sketch[Double])] = (0, sketch) :: dataIdxs.flatMap { case (data, idx) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO.map(sketch => (idx + 1, sketch))
    }
    val plots = idxUtdSketches.flatMap { case (idx, utdSkt) => utdSkt.densityPlot.map(plot => (idx, plot)) }

    ExpOutOps.clear(expName)
    ExpOutOps.writePlots(expName, plots)
  }

}
