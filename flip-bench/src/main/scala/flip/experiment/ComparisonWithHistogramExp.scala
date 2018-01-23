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

    val (histogram, histoConf) = self.histogram(histoSamplingNo)
    val (sketch, sketchConf) = self.sketch(sketchSamplingNo)

    // update datas
    val utdHistos = update(histogram, datas, 10, histoConf)
    val utdSketchs = update(sketch, datas, 10, sketchConf)

    // histogram results
    val histoPdf = utdHistos.flatMap { case (idx, _histo) => pdfResult(_histo, histoConf).map(pdf => (idx, pdf)) }
    val histoKlds = utdHistos.flatMap { case (idx, _histo) =>
      kldResult(_histo, underlying, histoConf).map((idx, _))
    }
    val histoKldd = histoKlds.map { case (idx, (kldd, _)) => (idx, kldd) }
    val histoKld = histoKlds.map { case (idx, (_, kld)) => (idx, kld) }
    val histoCosines = utdSketchs.flatMap { case (idx, _histo) =>
      cosineResult(_histo, underlying, histoConf).map((idx, _))
    }
    val histoCosd = histoCosines.map { case (idx, (cosd, _)) => (idx, cosd) }
    val histoCos = histoCosines.map { case (idx, (_, cos)) => (idx, cos) }

    // sketch results
    val sketchPdf = utdSketchs.flatMap { case (idx, _sketch) => pdfResult(_sketch, sketchConf).map(pdf => (idx, pdf)) }
    val sketchKlds = utdSketchs.flatMap { case (idx, _sketch) =>
      kldResult(_sketch, underlying, sketchConf).map((idx, _))
    }
    val sketchKldd = sketchKlds.map { case (idx, (kldd, _)) => (idx, kldd) }
    val sketchKld = sketchKlds.map { case (idx, (_, kld)) => (idx, kld) }
    val sketchCosines = utdSketchs.flatMap { case (idx, _sketch) =>
      cosineResult(_sketch, underlying, sketchConf).map((idx, _))
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
      lastSketchKld <- sketchKld.lastOption
      (idx1, histoKld) = lastHistoKld
      (idx2, sketchKld) = lastSketchKld
    } yield if(idx1 == idx2)
      println(s"KLD for $idx1 data: \n" +
        s" Histogram($histoSamplingNo): $histoKld \n" +
        s" Sketch($sketchSamplingNo): $sketchKld")
  }

  def histogram(no: Int): (Histogram[Double], HistogramConf) = {
    implicit val histoConf: HistogramConf = HistogramConf(
      binNo = no, start = -3.0, end = 3.0,
      counterSize = no
    )

    (Histogram.empty[Double], histoConf)
  }

  def sketch(no: Int): (Sketch[Double], SketchConf) = {
    implicit val sketchConf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100, boundaryCorr = 0.01, decayFactor = 0,
      queueSize = 20,
      cmapSize = no, cmapNo = 5, cmapStart = Some(-3.0), cmapEnd = Some(3.0),
      counterSize = no
    )

    (Sketch.empty[Double], sketchConf)
  }

  def update(sketch: Sketch[Double],
             datas: List[Double],
             period: Int,
             conf: SketchConf): List[(Int, Sketch[Double])] = {
    implicit val confImpl: SketchConf = conf
    val dataIdxs = datas.zipWithIndex
    var tempSketchO: Option[Sketch[Double]] = Option(sketch)

    (0, sketch) :: dataIdxs.flatMap { case (data, idx) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO.map(tempSketch => (idx + 1, tempSketch))
    }.filter { case (idx, _) => idx % period == 0 }
  }

  def pdfResult(sketch: Sketch[Double], conf: SketchConf): Option[DensityPlot] = for {
    pdf <- sketch.sampling(conf)
  } yield pdf

  /**
    * @return (kld density, kld)
    * */
  def kldResult(sketch: Sketch[Double],
                underlying: SmoothDist[Double],
                conf: SketchConf): Option[(DensityPlot, Double)] = {
    val samplingStart = -1.5
    val samplingEnd = 1.5

    for {
      underlyingSampling <- underlying.uniformSampling(samplingStart, samplingEnd, 100)
      kldd <- KLDDensity(underlyingSampling, sketch)(conf)
      kld <- KLD(underlyingSampling, sketch)(conf)
    } yield (kldd, kld)
  }

  /**
    * @return (consine density, cosine sim)
    * */
  def cosineResult(sketch: Sketch[Double],
                   underlying: SmoothDist[Double],
                   conf: SketchConf): Option[(DensityPlot, Double)] = {
    val samplingStart = -1.5
    val samplingEnd = 1.5

    for {
      underlyingSampling <- underlying.uniformSampling(samplingStart, samplingEnd, 100)
      cosd <- CosineDensity(underlyingSampling, sketch)(conf)
      cos <- Cosine(underlyingSampling, sketch)(conf)
    } yield (cosd, cos)
  }

}
