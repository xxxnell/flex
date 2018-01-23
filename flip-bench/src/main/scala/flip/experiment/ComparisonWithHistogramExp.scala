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

    val utdHistos = update(histogram, datas, 10, histoConf)
    val utdSketchs = update(sketch, datas, 10, sketchConf)

    val histoResults = utdHistos.traverse { case (idx, utdHisto) =>
      results(utdHisto, datas, underlying, histoConf).map(result => (idx, result))
    }.getOrElse(Nil)
    val sketchResults = utdSketchs.traverse { case (idx, utdSkt) =>
      results(utdSkt, datas, underlying, sketchConf).map(result => (idx, result))
    }.getOrElse(Nil)


    val histoPdf = histoResults.map { case (idx, (pdf, _, _)) => (idx, pdf) }
    val histoKldd = histoResults.map { case (idx, (_, kldd, _)) => (idx, kldd) }
    val histoKld = histoResults.map { case (idx, (_, _, kld)) => (idx, kld) }
    val sketchPdf = sketchResults.map { case (idx, (pdf, _, _)) => (idx, pdf) }
    val sketchKldd = sketchResults.map { case (idx, (_, kldd, _)) => (idx, kldd) }
    val sketchKld = sketchResults.map { case (idx, (_, _, kld)) => (idx, kld) }

    ExpOutOps.clear(expName)
    ExpOutOps.writePlots(expName, "histo-pdf", histoPdf)
    ExpOutOps.writePlots(expName, "histo-kld-density", histoKldd)
    ExpOutOps.writeStr(expName, "histo-kld", histoKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writePlots(expName, "sketch-pdf", sketchPdf)
    ExpOutOps.writePlots(expName, "sketch-kld-density", sketchKldd)
    ExpOutOps.writeStr(expName, "sketch-kld", sketchKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))

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

  /**
    * @return (pdf, kld density, kld)
    * */
  def results(sketch: Sketch[Double],
              datas: List[Double],
              underlying: SmoothDist[Double],
              conf: SketchConf): Option[(DensityPlot, DensityPlot, Double)] = {
    implicit val confImpl: SketchConf = conf
    val minDomainCutoff = -10e10
    val maxDomainCutoff = 10e10
    val samplingStart = -1.5
    val samplingEnd = 1.5

    for {
      // pdf
      pdf <- sketch.sampling(conf)
      // kld
      underlyingSampling <- underlying.uniformSampling(samplingStart, samplingEnd, 100)
      underlyingFiltered = underlyingSampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
      kldDensity <- KLDDensity(underlyingFiltered, sketch)(conf)
      kld <- KLD(underlyingFiltered, sketch)(conf)
    } yield (pdf, kldDensity, kld)
  }

}
