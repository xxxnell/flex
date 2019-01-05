package flex.experiment.core

import flex._
import flex.experiment.ops.ExpOutOps

object BasicBimodalDistExp {

  def main(args: Array[String]): Unit = {
    val expName = "basic-bimodal"
    val dataNo = 1000

    val sketch0 = {
      implicit val conf: SketchConf = SketchConf(
        cmapStart = Some(-10d),
        cmapEnd = Some(10)
      )
      Sketch.empty[Double]
    }
    val underlying = (0.5, NumericDist.normal(-2.0, 1)) + (0.5, NumericDist.normal(2.0, 1))
    val (_, datas) = underlying.samples(dataNo)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxPdf = idxSketches.map { case (idx, sketch)                                      => (idx, sketch.rangePdfSampling.csv) }
    val idxCdf = idxSketches.map { case (idx, sketch)                                      => (idx, sketch.cdfSampling.csv) }
    val idxDel = idxSketches.map { case (idx, sketch)                                      => (idx, Delta(underlying, sketch).csv) }
    val idxKld = idxSketches.map { case (idx, sketch)                                      => (idx, KLD(underlying, sketch)) }
    val idxCos = idxSketches.map { case (idx, sketch)                                      => (idx, Cosine(underlying, sketch)) }
    val idxEuc = idxSketches.map { case (idx, sketch)                                      => (idx, Euclidean(underlying, sketch)) }
    val idxED = idxSketches.map { case (idx, sketch)                                       => (idx, ED(underlying, sketch)) }

    ExpOutOps.clear(expName)
    ExpOutOps.writeStrs(expName, "pdf", idxPdf)
    ExpOutOps.writeStrs(expName, "cdf", idxCdf)
    ExpOutOps.writeStrs(expName, "delta", idxDel)
    ExpOutOps.writeStr(expName, "kld", idxKld.map { case (idx, kld)       => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "cosine", idxCos.map { case (idx, cosine) => s"$idx, $cosine" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "ed", idxED.map { case (idx, ed)          => s"$idx, $ed" }.mkString("\n"))
  }

}
