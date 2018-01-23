package flip.experiment

import flip._
import flip.pdf.SmoothDist
import cats.implicits._
import flip.experiment.ops.ExpOutOps

/**
  * A experiment to compare with sketch and histogram.
  * */
object ComparisonWithHistogramExp { self =>

  def main(args: Array[String]): Unit = {
    val expName = "histogram-comparison"
    val sampleNo = 1000
    val histoSamplingNo = 20
    val sketchSamplingNo = 20
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(sampleNo)

    val histogram = self.histogram(histoSamplingNo)
    val sketch = self.sketch(sketchSamplingNo)

    // update datas
    val utdHistos = update(histogram, datas, 10)
    val utdSketchs = update(sketch, datas, 10)

    // histogram results
    val histoPdf = utdHistos.flatMap { case (idx, _histo) => pdfResult(_histo).map(pdf => (idx, pdf)) }
    val histoKlds = utdHistos.flatMap { case (idx, _histo) =>
      kldResult(_histo, underlying).map((idx, _))
    }
    val histoKldd = histoKlds.map { case (idx, (kldd, _)) => (idx, kldd) }
    val histoKld = histoKlds.map { case (idx, (_, kld)) => (idx, kld) }
    val histoCosines = utdHistos.flatMap { case (idx, _histo) =>
      cosineResult(_histo, underlying).map((idx, _))
    }
    val histoCosd = histoCosines.map { case (idx, (cosd, _)) => (idx, cosd) }
    val histoCos = histoCosines.map { case (idx, (_, cos)) => (idx, cos) }

    // sketch results
    val sketchPdf = utdSketchs.flatMap { case (idx, _sketch) => pdfResult(_sketch).map(pdf => (idx, pdf)) }
    val sketchKlds = utdSketchs.flatMap { case (idx, _sketch) =>
      kldResult(_sketch, underlying).map((idx, _))
    }
    val sketchKldd = sketchKlds.map { case (idx, (kldd, _)) => (idx, kldd) }
    val sketchKld = sketchKlds.map { case (idx, (_, kld)) => (idx, kld) }
    val sketchCosines = utdSketchs.flatMap { case (idx, _sketch) =>
      cosineResult(_sketch, underlying).map((idx, _))
    }
    val sketchCosd = sketchCosines.map { case (idx, (cosd, _)) => (idx, cosd) }
    val sketchCos = sketchCosines.map { case (idx, (_, cos)) => (idx, cos) }


    ExpOutOps.clear(expName)

    // write histo results
    ExpOutOps.writePlots(expName, "histo-pdf", histoPdf)
    ExpOutOps.writePlots(expName, "histo-kld-density", histoKldd)
    ExpOutOps.writeStr(expName, "histo-kld", histoKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writePlots(expName, "histo-cos-density", histoCosd)
    ExpOutOps.writeStr(expName, "histo-cos", histoCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))

    // write sketch results
    ExpOutOps.writePlots(expName, "sketch-pdf", sketchPdf)
    ExpOutOps.writePlots(expName, "sketch-kld-density", sketchKldd)
    ExpOutOps.writeStr(expName, "sketch-kld", sketchKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writePlots(expName, "sketch-cos-density", sketchCosd)
    ExpOutOps.writeStr(expName, "sketch-cos", sketchCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))

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
    } yield if(idxH1 == idxS1 && idxH2 == idxS2) {
      val str =
        s"Simimarity for $idxH1 data: \n" +
          s" KLD(Histogram($histoSamplingNo)): $histoKld \n" +
          s" Cosine(Histogram($histoSamplingNo)): $histoCos \n" +
          s" KLD(Sketch($sketchSamplingNo)): $sketchKld \n" +
          s" Cosine(Sketch($sketchSamplingNo)): $sketchCos"

      println(str)
    } else println("Error occurs.")
  }

  def histogram(no: Int): Histogram[Double] = {
    implicit val histoConf: HistogramConf = HistogramConf(
      binNo = no, start = -3.0, end = 3.0,
      counterSize = no
    )

    Histogram.empty[Double]
  }

  def sketch(no: Int): Sketch[Double] = {
    implicit val sketchConf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100, boundaryCorr = 0.01, decayFactor = 0,
      queueSize = 20,
      cmapSize = no, cmapNo = 5, cmapStart = Some(-3.0), cmapEnd = Some(3.0),
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

  def pdfResult(sketch: Sketch[Double]): Option[DensityPlot] = for {
    pdf <- sketch.sampling
  } yield pdf

  /**
    * @return (kld density, kld)
    * */
  def kldResult(sketch: Sketch[Double],
                underlying: SmoothDist[Double]): Option[(DensityPlot, Double)] = {
    val samplingStart = -1.5
    val samplingEnd = 1.5

    for {
      underlyingSampling <- underlying.uniformSampling(samplingStart, samplingEnd, 100)
      kldd <- KLDDensity(underlyingSampling, sketch)
      kld <- KLD(underlyingSampling, sketch)
    } yield (kldd, kld)
  }

  /**
    * @return (consine density, cosine sim)
    * */
  def cosineResult(sketch: Sketch[Double],
                   underlying: SmoothDist[Double]): Option[(DensityPlot, Double)] = {
    val samplingStart = -1.5
    val samplingEnd = 1.5

    for {
      underlyingSampling <- underlying.uniformSampling(samplingStart, samplingEnd, 100)
      cosd <- CosineDensity(underlyingSampling, sketch)
      cos <- Cosine(underlyingSampling, sketch)
    } yield (cosd, cos)
  }

}
