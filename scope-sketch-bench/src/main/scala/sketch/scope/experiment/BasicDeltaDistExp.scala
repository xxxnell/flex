package sketch.scope.experiment

import sketch.scope.{ExpOutOps, _}
import sketch.scope.plot.DensityPlot

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

    var tempSketchO: Option[Sketch[Double]] = Option(sketch)
    val utdSketches: List[Option[Sketch[Double]]] = Option(sketch) :: datas.map { data =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO
    }
    val plots = utdSketches.map { sketchO => sketchO.flatMap(_.densityPlot) }.map(_.getOrElse(DensityPlot.empty))

    ExpOutOps.clear(expName)
    ExpOutOps.writePlots(expName, plots)
  }

}
