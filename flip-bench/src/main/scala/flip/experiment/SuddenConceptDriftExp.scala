package flip.experiment

import flip._
import flip.experiment.ops.{ComparisonOps, ExpOutOps}

/**
  * A experiment for sudden concept drift.
  * https://edouardfouche.com/img/concept-drift/conceptdrift.png
  * */
object SuddenConceptDriftExp {

  def main(args: Array[String]): Unit = {
    val expName = "sudden-cd-normal"
    val dataNo = 1000
    val draftStart = 300
    val start = 50
    val period = 100
    val samplingNo = 20

    implicit val conf: SketchConf = SketchConf(
      decayFactor = 1,
      startThreshold = start,
      thresholdPeriod = period,
      queueSize = 30,
      cmapSize = samplingNo,
      cmapNo = 5,
      cmapStart = Some(-10d),
      cmapEnd = Some(10),
      counterSize = 1000,
      counterNo = 2
    )
    val sketch0 = Sketch.empty[Double]
    val (mean1, mean2) = (0.0, 5.0)
    val underlying1 = NumericDist.normal(mean1, 1)
    val (_, datas1) = underlying1.samples(draftStart)
    val underlying2 = NumericDist.normal(mean2, 1)
    val (_, datas2) = underlying2.samples(dataNo)
    def underlying(idx: Int) = if (idx < draftStart) underlying1 else underlying2
    def center(idx: Int) = if (idx < draftStart) mean1 else mean2
    val datas = datas1 ++ datas2
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList
    val idxDensityPlots = idxSketches.map { case (idx, utdSkt) => (idx, utdSkt.pdfPlot) }
    val idxKld = idxSketches.map {
      case (idx, utdSkt) => (idx, ComparisonOps.identicalDomain(underlying(idx), utdSkt, KLD[Double]))
    }
    val idxCos = idxSketches.map {
      case (idx, utdSkt) => (idx, ComparisonOps.identicalDomain(underlying(idx), utdSkt, Cosine[Double]))
    }
    val idxEuc = idxSketches.map {
      case (idx, utdSkt) => (idx, ComparisonOps.identicalDomain(underlying(idx), utdSkt, Euclidean[Double]))
    }
    val idxSktMedian = idxSketches.map { case (idx, skt) => (idx, skt.median) }

    ExpOutOps.clear(expName)
    ExpOutOps.writePlots(expName, "pdf", idxDensityPlots)
    ExpOutOps.writeStr(expName, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "cosine", idxCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
    ExpOutOps.writeStr(
      expName,
      "median",
      idxSktMedian.map { case (idx, sktMed) => s"$idx, ${center(idx)}, $sktMed" }.mkString("\n"))
  }

}
