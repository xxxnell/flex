package flip.experiment

import flip.implicits._
import flip.experiment.ops.ExpOutOps

object BasicMapExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-map"
    val dataNo = 300

    implicit val conf: SketchConf = SketchConf(
      cmapStart = Some(-10),
      cmapEnd = Some(10)
    )
    val sketch0 = Sketch.empty[Double]
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList
    val (_, sketch1) = idxSketches.lastOption.getOrElse((0, sketch0))

    val mapped = sketch1.map(x => math.exp(x))
    val prevSketchPdf = sketch1.barPlot
    val bindingSketchPdf = mapped.barPlot

    // out

    ExpOutOps.clear(expName)
    ExpOutOps.writePlot(expName, "prev-pdf", prevSketchPdf)
    ExpOutOps.writePlot(expName, "post-pdf", bindingSketchPdf)
  }

}
