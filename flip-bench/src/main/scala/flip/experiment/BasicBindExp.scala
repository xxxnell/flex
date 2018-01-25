package flip.experiment

import flip.experiment.ops.{DataOps, ExpOutOps}
import flip._

/**
  * A basic experiment to bind lognormal with normal distribution.
  * */
object BasicBindExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-bind-lognormal"
    val dataNo = 300
    val samplingNo = 100

    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100, boundaryCorr = 0.1, decayFactor = 0,
      queueSize = 30,
      cmapSize = samplingNo, cmapNo = 5, cmapStart = Some(-10d), cmapEnd = Some(10),
      counterSize = samplingNo
    )
    val sketch0 = Sketch.empty[Double]
    val underlying = NumericDist.logNormal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val idxDatas = datas.indices.zip(datas).toList

    val idxSketches1 = DataOps.update(sketch0, idxDatas)
    val (_, sketch1) = idxSketches1.lastOption.getOrElse((0, sketch0))

    val bindingSketch = sketch1.flatMap(x => NumericDist.normal(x, 2))
    val prevSketchPdfO = sketch1.pdfPlot
    val bindingSketchPdfO = bindingSketch.pdfPlot

    // out

    ExpOutOps.clear(expName)

    for {
      prevPdf <- prevSketchPdfO
      postPdf <- bindingSketchPdfO
    } yield {
      ExpOutOps.writePlot(expName, "prev-pdf", prevPdf)
      ExpOutOps.writePlot(expName, "post-pdf", postPdf)
    }
  }

}
