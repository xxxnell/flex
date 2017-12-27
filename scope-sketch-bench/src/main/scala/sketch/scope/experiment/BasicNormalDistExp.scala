package sketch.scope.experiment

import sketch.scope.ExpOutOps
import sketch.scope.conf.{CmapConf, CounterConf, SketchConf}
import sketch.scope.measure.doubleMeasure
import sketch.scope.pdf.{Dist, PeriodicSketch, Sketch}
import sketch.scope.plot.DensityPlot

/**
  * Licensed by Probe Technology, Inc.
  */
object BasicNormalDistExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "basic-normal"

    val (cmapSize, cmapNo, cmapMin, cmapMax) = (150, 2, -10, 10)
    val (counterSize, counterNo) = (1000, 2)
    val sampleNo = 1000
    val start = 50
    val period = 100

    val conf: SketchConf = SketchConf(
      CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
      CounterConf(counterSize, counterNo)
    )
    val sketch = PeriodicSketch.emptyForPeriod(start, period)(doubleMeasure, conf)
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
