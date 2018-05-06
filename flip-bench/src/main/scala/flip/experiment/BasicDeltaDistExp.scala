package flip.experiment

import flip.experiment.ops.ExpOutOps
import flip._

object BasicDeltaDistExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-delta"
    val dataNo = 10
    val samplingNo = 100

    implicit val conf: SketchConf = SketchConf(
      startThreshold = 2,
      thresholdPeriod = 1,
      cmapSize = samplingNo,
      cmapNo = 2,
      cmapStart = Some(-10d),
      cmapEnd = Some(10d),
      counterSize = samplingNo
    )
    val sketch0 = Sketch.empty[Double]
    val (_, datas) = Dist.delta(0.1).samples(dataNo)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxPlots = idxSketches.map { case (idx, utdSkt) => (idx, utdSkt.barPlot) }

    ExpOutOps.clear(expName)
    ExpOutOps.writePlots(expName, "pdf", idxPlots)
  }

}
