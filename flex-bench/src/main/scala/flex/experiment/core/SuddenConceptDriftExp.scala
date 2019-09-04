package flex.experiment.core

import flex.experiment.ops.ExpOutOps
import flex.implicits._

/**
 * A experiment for sudden concept drift.
 * https://edouardfouche.com/img/concept-drift/conceptdrift.png
 * */
object SuddenConceptDriftExp {

  def main(args: Array[String]): Unit = {
    val expName = "sudden-cd-normal"
    val dataNo = 1000
    val draftStart = 300

    implicit val conf: SketchConf = SketchConf(cmapStart = Some(-10), cmapEnd = Some(10))
    val sketch0 = Sketch.empty[Double]
    val (mean1, mean2) = (0.0, 5.0)
    def center(idx: Int) = if (idx < draftStart) mean1 else mean2
    def underlying(idx: Int) = NumericDist.normal(center(idx), 1, idx)
    val datas = (1 to dataNo).map(idx => underlying(idx).sample._2).toList
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxPdf = idxSketches.map { case (idx, sketch) => (idx, sketch.barPlot.csv) }
    val idxCdf = idxSketches.map { case (idx, sketch) => (idx, sketch.cdfSampling.csv) }
    val idxDel = idxSketches.map { case (idx, sketch) => (idx, Delta(underlying(idx), sketch).csv) }
    val idxKld = idxSketches.map { case (idx, sketch) => (idx, KLD(underlying(idx), sketch)) }
    val idxCos = idxSketches.map { case (idx, sketch) => (idx, Cosine(underlying(idx), sketch)) }
    val idxEuc = idxSketches.map { case (idx, sketch) => (idx, Euclidean(underlying(idx), sketch)) }
    val idxED = idxSketches.map { case (idx, sketch) => (idx, ED(underlying(idx), sketch)) }
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
