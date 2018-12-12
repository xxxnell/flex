package flex.experiment

import flex.implicits._
import flex.experiment.ops.ExpOutOps

object BasicParetoDistExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "basic-pareto"
    val dataNo = 1000

    implicit val conf: SketchConf = SketchConf(
      cmapStart = Some(-10),
      cmapEnd = Some(10)
    )
    val sketch0 = Sketch.empty[Double]
    val underlying = NumericDist.pareto(1d, 1d)
    val (_, datas) = underlying.samples(dataNo)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxDensityPlots = idxSketches.map { case (idx, utdSkt) => (idx, utdSkt.barPlot) }
    val idxKld = idxSketches.map { case (idx, utdSkt) => (idx, KLD(underlying, utdSkt)) }
    val idxCos = idxSketches.map { case (idx, utdSkt) => (idx, Cosine(underlying, utdSkt)) }
    val idxEuc = idxSketches.map { case (idx, utdSkt) => (idx, Euclidean(underlying, utdSkt)) }

    ExpOutOps.clear(expName1)
    ExpOutOps.writePlots(expName1, "pdf", idxDensityPlots)
    ExpOutOps.writeStr(expName1, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName1, "cosine", idxCos.map { case (idx, cosine) => s"$idx, $cosine" }.mkString("\n"))
    ExpOutOps.writeStr(expName1, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
  }

}
