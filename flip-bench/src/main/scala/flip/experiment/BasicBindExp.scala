package flip.experiment

import flip.experiment.ops.ExpOutOps
import flip._

/**
  * A basic experiment to bind lognormal with normal distribution.
  * */
object BasicBindExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-bind"
    val dataNo = 300
    val samplingNo = 20

    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50,
      thresholdPeriod = 100,
      boundaryCorr = 0.1,
      decayFactor = 0,
      queueSize = 30,
      cmapSize = samplingNo,
      cmapNo = 5,
      cmapStart = Some(-10d),
      cmapEnd = Some(10),
      counterSize = samplingNo
    )
    val sketch0 = Sketch.empty[Double]
    val underlying = NumericDist.logNormal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList
    val (_, sketch1) = idxSketches.lastOption.getOrElse((0, sketch0))

    val bindingSketch = sketch1.flatMap(x => NumericDist.normal(x, 2))
    val prevSketchPdf = sketch1.pdfPlot
    val bindingSketchPdf = bindingSketch.pdfPlot

    // out

    ExpOutOps.clear(expName)
    ExpOutOps.writePlot(expName, "prev-pdf", prevSketchPdf)
    ExpOutOps.writePlot(expName, "post-pdf", bindingSketchPdf)
  }

}
