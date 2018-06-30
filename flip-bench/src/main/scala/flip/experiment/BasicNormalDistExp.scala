package flip.experiment

import flip.implicits._
import flip.experiment.ops.ExpOutOps
import flip.plot.PointPlot

object BasicNormalDistExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "basic-normal"
    val dataNo = 1000

    implicit val conf: SketchConf = SketchConf(
      cmapStart = Some(-20),
      cmapEnd = Some(20),
      rebuildThreshold = 0.2
    )
    val sketch0 = Sketch.empty[Double]
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxPdf = idxSketches.map { case (idx, sketch) => (idx, sketch.rangePdfSampling.csv) }
    val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }
    val idxDel = idxSketches.map { case (idx, sketch) => (idx, Delta(underlying, sketch).csv) }
    val idxKld = idxSketches.map { case (idx, sketch) => (idx, KLD(underlying, sketch)) }
    val idxCos = idxSketches.map { case (idx, sketch) => (idx, Cosine(underlying, sketch)) }
    val idxEuc = idxSketches.map { case (idx, sketch) => (idx, Euclidean(underlying, sketch)) }
    val idxED = idxSketches.map { case (idx, sketch) => (idx, ED(underlying, sketch)) }

    ExpOutOps.clear(expName1)
    ExpOutOps.writeStrs(expName1, "pdf", idxPdf)
    ExpOutOps.writeStrs(expName1, "cdf", idxCdf)
    ExpOutOps.writeStrs(expName1, "delta", idxDel)
    ExpOutOps.writeStr(expName1, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName1, "cosine", idxCos.map { case (idx, cosine) => s"$idx, $cosine" }.mkString("\n"))
    ExpOutOps.writeStr(expName1, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
    ExpOutOps.writeStr(expName1, "ed", idxED.map { case (idx, ed) => s"$idx, $ed" }.mkString("\n"))
  }

}
