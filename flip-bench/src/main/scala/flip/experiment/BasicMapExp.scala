package flip.experiment

import flip.implicits._
import flip.experiment.ops.ExpOutOps

object BasicMapExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-map"
    val dataNo = 300
    val samplingNo = 20

    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50,
      thresholdPeriod = 100,
      decayFactor = 0,
      queueSize = 30,
      cmapSize = samplingNo,
      cmapNo = 5,
      cmapStart = Some(-10d),
      cmapEnd = Some(10),
      boundaryRatio = 0.1,
      counterSize = samplingNo
    )
    val sketch0 = Sketch.empty[Double]
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList
    val (_, sketch1) = idxSketches.lastOption.getOrElse((0, sketch0))

    val mapped = sketch1.map(x => math.exp(x))
    val prevSketchPdf = sketch1.pdfPlot
    val bindingSketchPdf = mapped.pdfPlot

    // out

    ExpOutOps.clear(expName)
    ExpOutOps.writePlot(expName, "prev-pdf", prevSketchPdf)
    ExpOutOps.writePlot(expName, "post-pdf", bindingSketchPdf)
  }

}
