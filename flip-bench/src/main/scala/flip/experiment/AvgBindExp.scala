package flip.experiment

import flip.experiment.ops.ExpOutOps
import flip.implicits.{NumericDist, Sketch, SketchConf}

object AvgBindExp {

  def main(args: Array[String]): Unit = {
    val expName = "avg-bind"
    val dataNo = 300

    implicit val conf: SketchConf = SketchConf(
      cmapStart = Some(-10d),
      cmapEnd = Some(10)
    )
    val sketchA0 = Sketch.empty[Double]
    val sketchB0 = Sketch.empty[Double]
    val sketchC0 = Sketch.empty[Double]
    val underlyingA = NumericDist.normal(50.0, 20)
    val underlyingB = NumericDist.normal(70.0, 15)
    val underlyingC = NumericDist.normal(100.0, 10)
    val (_, datasA) = underlyingA.samples(dataNo)
    val (_, datasB) = underlyingB.samples(dataNo)
    val (_, datasC) = underlyingC.samples(dataNo)
    val sketchA1 = sketchA0.updateInOrder(datasA)
    val sketchB1 = sketchB0.updateInOrder(datasB)
    val sketchC1 = sketchC0.updateInOrder(datasC)

    val bindingSketch = for {
      a1 <- sketchA1
      b1 <- sketchB1
      c1 <- sketchC1
    } yield (a1 + b1 + c1) / 3

    val prevSketchAPdf = sketchA1.pdfPlot
    val prevSketchBPdf = sketchB1.pdfPlot
    val prevSketchCPdf = sketchC1.pdfPlot
    val bindingSketchPdf = bindingSketch.pdfPlot

    // out

    ExpOutOps.clear(expName)
    ExpOutOps.writePlot(expName, "prev-pdf-a", prevSketchAPdf)
    ExpOutOps.writePlot(expName, "prev-pdf-b", prevSketchBPdf)
    ExpOutOps.writePlot(expName, "prev-pdf-c", prevSketchCPdf)
    ExpOutOps.writePlot(expName, "post-pdf", bindingSketchPdf)
  }

}
