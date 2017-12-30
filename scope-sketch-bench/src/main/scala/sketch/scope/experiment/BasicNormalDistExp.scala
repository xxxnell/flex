package sketch.scope.experiment

import sketch.scope.{ExpOutOps, _}

/**
  * Licensed by Probe Technology, Inc.
  */
object BasicNormalDistExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "basic-normal"
    val sampleNo = 1000
    val start = 50
    val period = 100

    implicit val conf: SketchConf = SketchConf(
      startThreshold = start, thresholdPeriod = period,
      cmapSize = 150, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10),
      counterSize = 1000, counterNo = 2
    )
    val sketch = Sketch.empty[Double]
    val underlying = Dist.normal(0.1, 1)
    val (_, datas) = underlying.samples(sampleNo)
    val dataIdxs = datas.zipWithIndex

    var tempSketchO: Option[Sketch[Double]] = Option(sketch)
    val idxUtdSketches: List[(Int, Sketch[Double])] = (0, sketch) :: dataIdxs.flatMap { case (data, idx) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO.map(tempSketch => (idx + 1, tempSketch))
    }.filter { case (idx, _) => idx % period == 0 }
    val idxDensityPlots = idxUtdSketches.flatMap { case (idx, utdSkt) => utdSkt.densityPlot.map(plot => (idx, plot)) }
    val idxKldPlot = idxUtdSketches.flatMap { case (idx, utdSkt) =>
      KLDDensity(utdSkt, underlying).map(plot => (idx, plot))
    }
    val idxKld = idxUtdSketches.flatMap { case (idx, utdSkt) => KLD(utdSkt, underlying).map(kld => (idx, kld)) }

    ExpOutOps.clear(expName1)
    ExpOutOps.writePlots(expName1, idxDensityPlots)
    ExpOutOps.writePlots(expName1, "kld", idxKldPlot)
  }

}
