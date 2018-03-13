package flip.experiment

import flip._
import flip.experiment.ops.ExpOutOps

/**
  * A experiment to compare with sketch and histogram.
  * */
object HistogramBimodalDistExp { self =>

  def main(args: Array[String]): Unit = {
    val expName = "histogram-bimodal"
    val sampleNo = 1000
    val samplingNo = 20
    val underlying = { (0.5, NumericDist.normal(-2.0, 1)) + (0.5, NumericDist.normal(2.0, 1)) }
    val (_, datas) = underlying.samples(sampleNo)
    val idxDatas = (datas.indices zip datas).toList
    val start = -4.8
    val end = 4.8

    val emptyHisto = {
      implicit val histoConf: HistogramConf = HistogramConf(
        binNo = samplingNo,
        start = start,
        end = end,
        counterSize = samplingNo
      )
      Histogram.empty[Double]
    }

    // update datas
    val histoTraces = emptyHisto :: emptyHisto.updateTrace(datas)
    val idxHistos = histoTraces.indices.zip(histoTraces).toList.filter { case (idx, _) => idx % 10 == 0 }

    // histogram results
    val idxPdf = idxHistos.map { case (idx, histo) => (idx, histo.sampling) }
    val idxKld = idxHistos.map { case (idx, utdSkt) => (idx, KLD(underlying, utdSkt)) }
    val idxCos = idxHistos.map { case (idx, utdSkt) => (idx, Cosine(underlying, utdSkt)) }
    val idxEuc = idxHistos.map { case (idx, utdSkt) => (idx, Euclidean(underlying, utdSkt)) }

    ExpOutOps.clear(expName)

    // write histo results
    ExpOutOps.writePlots(expName, "histo-pdf", idxPdf)
    ExpOutOps.writeStr(expName, "histo-kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-cos", idxCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-euclidean", idxEuc.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
  }

}
