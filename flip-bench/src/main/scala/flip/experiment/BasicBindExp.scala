package flip.experiment

import flip.experiment.ops.ExpOutOps
import flip.implicits._

/**
  * A basic experiment to bind lognormal with normal distribution.
  * */
object BasicBindExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-bind"
    val dataNo = 300

    implicit val conf: SketchConf = SketchConf(
      cmapStart = Some(-10d),
      cmapEnd = Some(10)
    )
    val sketch0 = Sketch.empty[Double]
    val underlying = NumericDist.logNormal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val sketch1 = sketch0.updateInOrder(datas)

    val bindingSketch = sketch1.flatMap(x => NumericDist.normal(x, 1.5))
    val prevSketchPdf = sketch1.pdfPlot
    val bindingSketchPdf = bindingSketch.pdfPlot

    // out

    ExpOutOps.clear(expName)
    ExpOutOps.writePlot(expName, "prev-pdf", prevSketchPdf)
    ExpOutOps.writePlot(expName, "post-pdf", bindingSketchPdf)
  }

}
