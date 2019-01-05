package flex.experiment.core

import flex.conf.pdf.{CustomDataBinningDistConf, DataBinningDistConf}
import flex.experiment.ops.ExpOutOps
import flex.implicits._
import flex.pdf.Histogram

/**
 * A experiment to compare with sketch and histogram.
 * */
object HistogramLognormalDistExp { self =>

  def main(args: Array[String]): Unit = {
    val expName = "histogram-lognormal"
    val sampleNo = 1000
    val underlying = NumericDist.logNormal(0.0, 1)
    val (_, datas) = underlying.samples(sampleNo)
    val start = 0.0
    val end = underlying.icdf(0.95)

    implicit val histoConf: DataBinningDistConf = CustomDataBinningDistConf(
      cmapStart = Some(start),
      cmapEnd = Some(end)
    )
    val emptyHisto = Histogram.empty[Double]

    // update datas
    val histoTraces = emptyHisto :: emptyHisto.updateTrace(datas)
    val idxHistos = histoTraces.indices.zip(histoTraces).toList.filter { case (idx, _) => idx % 10 == 0 }

    // histogram results
    val idxPdf = idxHistos.map { case (idx, hist) => (idx, hist.barPlot) }
    val idxKld = idxHistos.map { case (idx, hist) => (idx, KLD(underlying, hist)) }
    val idxCos = idxHistos.map { case (idx, hist) => (idx, Cosine(underlying, hist)) }
    val idxEuc = idxHistos.map { case (idx, hist) => (idx, Euclidean(underlying, hist)) }

    ExpOutOps.clear(expName)

    // write histo results
    ExpOutOps.writePlots(expName, "histo-pdf", idxPdf)
    ExpOutOps.writeStr(expName, "histo-kld", idxKld.map { case (idx, kld)       => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-cos", idxCos.map { case (idx, cos)       => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-euclidean", idxEuc.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
  }

}
