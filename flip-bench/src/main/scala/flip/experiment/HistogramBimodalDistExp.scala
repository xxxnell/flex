package flip.experiment

import flip._
import flip.experiment.ops.{ComparisonOps, DataOps, ExpOutOps}

/**
  * A experiment to compare with sketch and histogram.
  * */
object HistogramBimodalDistExp { self =>

  def main(args: Array[String]): Unit = {
    val expName = "histogram-bimodal"
    val sampleNo = 1000
    val samplingNo = 20
    val underlying = (0.5, NumericDist.normal(-2.0, 1)) + (0.5, NumericDist.normal(2.0, 1))
    val (_, datas) = underlying.samples(sampleNo)
    val idxDatas = (datas.indices zip datas).toList

    val emptyHisto = self.histogram(samplingNo)

    // update datas
    val utdHistos = DataOps.update(emptyHisto, idxDatas)
      .filter { case (idx, _) => idx % 10 == 0 }

    // histogram results
    val histoPdf = utdHistos.flatMap { case (idx, histo) => histo.sampling.map((idx, _)) }
    val histoKld = utdHistos.flatMap { case (idx, histo) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, samplingNo * 3, histo, KLD[Double]).map((idx, _))
    }
    val histoCos = utdHistos.flatMap { case (idx, histo) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, samplingNo * 3, histo, Cosine[Double]).map((idx, _))
    }
    val histoEuc = utdHistos.flatMap { case (idx, histo) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, samplingNo * 3, histo, Euclidean[Double]).map((idx, _))
    }

    ExpOutOps.clear(expName)

    // write histo results
    ExpOutOps.writePlots(expName, "histo-pdf", histoPdf)
    ExpOutOps.writeStr(expName, "histo-kld", histoKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-cos", histoCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-euclidean", histoEuc.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
  }

  def histogram(no: Int): Histogram[Double] = {
    implicit val histoConf: HistogramConf = HistogramConf(
      binNo = no, start = -5.0, end = 5.0,
      counterSize = no
    )

    Histogram.empty[Double]
  }

}
