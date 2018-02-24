package flip.experiment

import flip._
import flip.experiment.ops.{DataOps, ExpOutOps}

object BasicMapExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-map"
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
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val idxDatas = datas.indices.zip(datas).toList

    val idxSketches1 = DataOps.update(sketch0, idxDatas)
    val (_, sketch1) = idxSketches1.lastOption.getOrElse((0, sketch0))

    val mapped = sketch1.map(x => math.exp(x))
    val prevSketchPdfO = sketch1.pdfPlot
    val bindingSketchPdfO = mapped.pdfPlot

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
