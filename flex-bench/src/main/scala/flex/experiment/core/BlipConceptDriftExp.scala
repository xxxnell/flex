package flex.experiment.core

import flex.experiment.ops.ExpOutOps
import flex.implicits._

object BlipConceptDriftExp {

  def main(args: Array[String]): Unit = {
    val expName = "blip-cd-normal"
    val dataNo = 600
    val draftStart = 300
    val duration = 3

    implicit val conf: SketchConf = SketchConf(cmapStart = Some(-10), cmapEnd = Some(10))
    val sketch0 = Sketch.empty[Double]
    val (mean1, mean2) = (0.0, 5.0)
    def center(idx: Int) = if (idx < draftStart) mean1 else if (idx < draftStart + duration) mean2 else mean1
    def underlying(idx: Int) = NumericDist.normal(center(idx), 1, idx)
    val underlying0 = NumericDist.normal(center(0), 1)
    val datas = (1 to dataNo).map(idx => underlying(idx).sample._2).toList
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter {
      case (idx, _) => idx >= draftStart - duration && idx <= draftStart + 2 * duration
    }
    val idxPdf = idxSketches.map { case (idx, sketch) => (idx, sketch.barPlot.csv) }
    val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }
    val idxDel = idxSketches.map { case (idx, sketch) => (idx, Delta(underlying0, sketch).csv) }
    val idxKld = idxSketches.map { case (idx, sketch) => (idx, KLD(underlying0, sketch)) }
    val idxCos = idxSketches.map { case (idx, sketch) => (idx, Cosine(underlying0, sketch)) }
    val idxEuc = idxSketches.map { case (idx, sketch) => (idx, Euclidean(underlying0, sketch)) }
    val idxED = idxSketches.map { case (idx, sketch) => (idx, ED(underlying0, sketch)) }
    val idxMedian = idxSketches.map { case (idx, sketch) => (idx, sketch.median) }

    ExpOutOps.clear(expName)
    ExpOutOps.writeStrs(expName, "pdf", idxPdf)
    ExpOutOps.writeStrs(expName, "cdf", idxCdf)
    ExpOutOps.writeStrs(expName, "delta", idxDel)
    ExpOutOps.writeStr(expName, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "cosine", idxCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "ed", idxED.map { case (idx, ed) => s"$idx, $ed" }.mkString("\n"))
    ExpOutOps.writeStr(
      expName,
      "median",
      idxMedian.map { case (idx, sktMed) => s"$idx, ${center(idx)}, $sktMed" }.mkString("\n"))
  }

}
