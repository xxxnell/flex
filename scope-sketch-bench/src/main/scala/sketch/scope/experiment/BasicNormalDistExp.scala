package sketch.scope.experiment

import sketch.scope.ExpOutOps
import sketch.scope._
import sketch.scope.pdf.PeriodicSketch
import sketch.scope.plot.DensityPlot

/**
  * Licensed by Probe Technology, Inc.
  */
object BasicNormalDistExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "basic-normal"
    val sampleNo = 1000
    val period = 100

    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = period,
      cmapSize = 150, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10),
      counterSize = 1000, counterNo = 2
    )
    val sketch = Sketch.empty[Double]
    val (_, datas) = Dist.normal(0.1, 1).samples(sampleNo)
    val dataIdxs = datas.zipWithIndex

    var mul = 0
    var tempSketchO: Option[Sketch[Double]] = Option(sketch)
    val utdSketches: List[Option[Sketch[Double]]] = Option(sketch) :: dataIdxs.flatMap { case (data, idx) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      if(idx / period > mul) {
        mul = idx / period
        Some(tempSketchO)
      } else None
    }
    val plots = utdSketches.map { sketchO => sketchO.flatMap(_.densityPlot) }.map(_.getOrElse(DensityPlot.empty))

    ExpOutOps.clear(expName1)
    ExpOutOps.writePlots(expName1, plots)
  }

}
