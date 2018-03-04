package flip.experiment

import flip.experiment.ops.ExpOutOps
import flip._
import flip.experiment.ops._

object BasicNormalDistExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "basic-normal"
    val dataNo = 1000
    val samplingNo = 20
    val start = 50
    val period = 100

    implicit val conf: SketchConf = SketchConf(
      startThreshold = start,
      thresholdPeriod = period,
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
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxDensityPlots = idxSketches.map { case (idx, utdSkt) => (idx, utdSkt.densityPlot) }
    val idxKld = idxSketches.map {
      case (idx, utdSkt) =>
        (idx, ComparisonOps.identicalDomain(underlying, utdSkt, KLD[Double]))
    }
    val idxCosine = idxSketches.map {
      case (idx, utdSkt) =>
        (idx, ComparisonOps.uniformDomain(underlying, -2.5, 2.5, samplingNo * 3, utdSkt, Cosine[Double]))
    }
    val idxEuclidean = idxSketches.map {
      case (idx, utdSkt) =>
        (idx, ComparisonOps.uniformDomain(underlying, -2.5, 2.5, samplingNo * 3, utdSkt, Euclidean[Double]))
    }

    ExpOutOps.clear(expName1)
    ExpOutOps.writePlots(expName1, "pdf", idxDensityPlots)
    ExpOutOps.writeStr(expName1, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName1, "cosine", idxCosine.map { case (idx, cosine) => s"$idx, $cosine" }.mkString("\n"))
    ExpOutOps.writeStr(expName1, "euclidean", idxEuclidean.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
  }

}
