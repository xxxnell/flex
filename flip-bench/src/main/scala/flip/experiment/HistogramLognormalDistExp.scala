package flip.experiment

import flip.implicits._
import flip.experiment.ops.ExpOutOps

/**
  * A experiment to compare with sketch and histogram.
  * */
object HistogramLognormalDistExp { self =>

  def main(args: Array[String]): Unit = {
    val expName = "histogram-lognormal"
    val sampleNo = 1000
    val samplingNo = 20
    val underlying = NumericDist.logNormal(0.0, 1)
    val (_, datas) = underlying.samples(sampleNo)
    val idxDatas = (datas.indices zip datas).toList
    val smplStart = 0.0
    val smplEnd = underlying.icdf(0.95)

    implicit val histoConf: HistogramConf = HistogramConf(
      binNo = samplingNo,
      start = smplStart,
      end = smplEnd,
      counterSize = samplingNo
    )
    val emptyHisto = Histogram.empty[Double]

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
