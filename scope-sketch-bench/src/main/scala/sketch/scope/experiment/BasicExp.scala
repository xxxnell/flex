package sketch.scope.experiment

import sketch.scope.ExpOutOps
import sketch.scope.conf.{CmapConf, CounterConf, SketchConf}
import sketch.scope.pdf._
import sketch.scope.measure._
import sketch.scope.plot.{DensityPlot, Plot}

/**
  * Licensed by Probe Technology, Inc.
  */
object BasicExp {

  val expName = "basic"

  def main(args: Array[String]): Unit = {
    val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
    val (counterSize, counterNo) = (8, 2)
    val sampleNo = 10

    val conf: SketchConf = SketchConf(
      CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
      CounterConf(counterSize, counterNo)
    )
    val sketch = PeriodicSketch.emptyForPeriod(0, 1)(doubleMeasure, conf)
    val (_, datas) = Dist.normal(0d, 1).samples(sampleNo)

    var tempSketchO: Option[Sketch[Double]] = Option(sketch)
    val utdSketches = datas.map { data =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO
    }
    val plots = utdSketches.map { sketchO => sketchO.flatMap(_.densityPlot) }.map(_.getOrElse(DensityPlot.empty))

    ExpOutOps.clear(expName)
    ExpOutOps.writePlots(expName, plots)
  }

}
