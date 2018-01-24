package flip.experiment

import flip._
import flip.experiment.ops.{ComparisonOps, ExpOutOps}

/**
  * A experiment to compare with sketch and histogram.
  * */
object ComparisonWithHistogramExp { self =>

  def main(args: Array[String]): Unit = {
    val expName = "histogram-comparison"
    val sampleNo = 1000
    val histoSamplingNo = 20
    val sketchSamplingNo = 20
    val underlying = (0.5, NumericDist.normal(-2.0, 1)) + (0.5, NumericDist.normal(2.0, 1))
    val (_, datas) = underlying.samples(sampleNo)

    val emptyHisto = self.histogram(histoSamplingNo)
    val emptySketch = self.sketch(sketchSamplingNo)

    // update datas
    val utdHistos = update(emptyHisto, datas, 10)
    val utdSketches = update(emptySketch, datas, 10)

    // histogram results
    val histoPdf = utdHistos.flatMap { case (idx, histo) => histo.sampling.map((idx, _)) }
    val histoKld = utdHistos.flatMap { case (idx, histo) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, histoSamplingNo * 3, histo, KLD[Double]).map((idx, _))
    }
    val histoCos = utdHistos.flatMap { case (idx, histo) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, histoSamplingNo * 3, histo, Cosine[Double]).map((idx, _))
    }
    val histoEuc = utdHistos.flatMap { case (idx, histo) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, histoSamplingNo * 3, histo, Euclidean[Double]).map((idx, _))
    }

    // sketch results
    val sketchPdf = utdSketches.flatMap { case (idx, sketch) => sketch.sampling.map((idx, _)) }
    val sketchKld = utdSketches.flatMap { case (idx, sketch) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, sketchSamplingNo * 3, sketch, KLD[Double]).map((idx, _))
    }
    val sketchCos = utdSketches.flatMap { case (idx, sketch) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, sketchSamplingNo * 3, sketch, Cosine[Double]).map((idx, _))
    }
    val sketchEuc = utdSketches.flatMap { case (idx, sketch) =>
      ComparisonOps.uniformDomain(underlying, -3.0, 3.0, sketchSamplingNo * 3, sketch, Euclidean[Double]).map((idx, _))
    }

    ExpOutOps.clear(expName)

    // write histo results
    ExpOutOps.writePlots(expName, "histo-pdf", histoPdf)
    ExpOutOps.writeStr(expName, "histo-kld", histoKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-cos", histoCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "histo-euclidean", histoEuc.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))

    // write sketch results
    ExpOutOps.writePlots(expName, "sketch-pdf", sketchPdf)
    ExpOutOps.writeStr(expName, "sketch-kld", sketchKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "sketch-cos", sketchCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "sketch-euclidean", sketchEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))

    // console print
    for {
      lastHistoKld <- histoKld.lastOption
      (idxH1, histoKld) = lastHistoKld
      lastSketchKld <- sketchKld.lastOption
      (idxS1, sketchKld) = lastSketchKld
      lastHistoCos <- histoCos.lastOption
      (idxH2, histoCos) = lastHistoCos
      lastSketchCos <- sketchCos.lastOption
      (idxS2, sketchCos) = lastSketchCos
      lastHistoEuc <- histoEuc.lastOption
      (idxH3, histoEuc) = lastHistoEuc
      lastSketchEuc <- sketchEuc.lastOption
      (idxS3, sketchEuc) = lastSketchEuc
    } yield if(idxH1 == idxS1 && idxH2 == idxS2 && idxH3 == idxS3) {
      val str =
        s"Simimarity for $idxH1 data: \n" +
          s" KLD(Histogram($histoSamplingNo)): $histoKld \n" +
          s" Cosine(Histogram($histoSamplingNo)): $histoCos \n" +
          s" Euclidean(Histogram($histoSamplingNo)): $histoEuc \n" +
          s" KLD(Sketch($sketchSamplingNo)): $sketchKld \n" +
          s" Cosine(Sketch($sketchSamplingNo)): $sketchCos\n" +
          s" Euclidean(Sketch($sketchSamplingNo)): $sketchEuc"

      println(str)
    } else println("Error occurs.")
  }

  def histogram(no: Int): Histogram[Double] = {
    implicit val histoConf: HistogramConf = HistogramConf(
      binNo = no, start = -5.0, end = 5.0,
      counterSize = no
    )

    Histogram.empty[Double]
  }

  def sketch(no: Int): Sketch[Double] = {
    implicit val sketchConf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100, boundaryCorr = 0.01, decayFactor = 0,
      queueSize = 20,
      cmapSize = no, cmapNo = 5, cmapStart = Some(-5.0), cmapEnd = Some(5.0),
      counterSize = no
    )

    Sketch.empty[Double]
  }

  def update(sketch: Sketch[Double],
             datas: List[Double],
             period: Int): List[(Int, Sketch[Double])] = {
    val dataIdxs = datas.zipWithIndex
    var tempSketchO: Option[Sketch[Double]] = Option(sketch)

    (0, sketch) :: dataIdxs.flatMap { case (data, idx) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO.map(tempSketch => (idx + 1, tempSketch))
    }.filter { case (idx, _) => idx % period == 0 }
  }

}
