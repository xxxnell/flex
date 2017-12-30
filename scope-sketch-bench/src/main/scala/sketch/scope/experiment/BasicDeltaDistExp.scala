package sketch.scope.experiment

import sketch.scope.ExpOutOps
import sketch.scope._
import sketch.scope.pdf.PeriodicSketch
import sketch.scope.plot.DensityPlot

/**
  * Licensed by Probe Technology, Inc.
  */
object BasicDeltaDistExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-delta"

    val (cmapSize, cmapNo, cmapMin, cmapMax) = (100, 2, -10, 10)
    val (counterSize, counterNo) = (1000, 2)
    val sampleNo = 10

    val conf: SketchConf = SketchConf(
      cmapSize, cmapNo, cmapMin, cmapMax,
      counterSize, counterNo
    )
    val sketch = PeriodicSketch.emptyForPeriod(2, 1)(doubleMeasure, conf)
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
