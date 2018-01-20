package flip.experiment

import flip._
import flip.pdf.SmoothDist

/**
  * A experiment to compare with sketch and histogram.
  * */
object ComparisonWithHistogramExp { self =>

  def main(args: Array[String]): Unit = {
    val sampleNo = 1000
    val histoSampleNo = 20
    val sketchSampleNo = 20
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(sampleNo)

    val (histogram, histoConf) = self.histogram(histoSampleNo)
    val (sketch, sketchConf) = self.sketch(sketchSampleNo)

    for {
      histoResult <- results(histogram, datas, underlying, histoConf)
      (histoPdf, histoKldDensity, histoKld) = histoResult
      sketchResult <- results(sketch, datas, underlying, sketchConf)
      (sketchPdf, sketchKldDensity, sketchKld) = sketchResult
    } yield {
      println(
        s"$sampleNo samples: \n" +
          s"  KLD for Histogram($histoSampleNo) = $histoKld \n" +
          s"  KLD for Sketch($sketchSampleNo) = $sketchKld"
      )
    }
  }

  def histogram(no: Int): (Histogram[Double], HistogramConf) = {
    implicit val histoConf: HistogramConf = HistogramConf(
      binNo = no, start = -2.5, end = 2.5,
      counterSize = 1000, counterNo = 2
    )

    (Histogram.empty[Double], histoConf)
  }

  def sketch(no: Int): (Sketch[Double], SketchConf) = {
    implicit val sketchConf: SketchConf = SketchConf(
      startThreshold = 100, thresholdPeriod = 50, boundaryCorr = 0.01, decayFactor = 0,
      queueSize = 100,
      cmapSize = no, cmapNo = 5, cmapStart = Some(-10d), cmapEnd = Some(10),
      counterSize = 1000, counterNo = 2
    )

    (Sketch.empty[Double], sketchConf)
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

    val utdSketchO = datas.foldLeft(Option(sketch)){ case (sketchO, data) =>
      for {
        sketch <- sketchO
        utd <- sketch.update(data)
      } yield utd
    }

    for {
      utdSketch <- utdSketchO
      // pdf
      pdf <- utdSketch.sampling
      // kld density
      underlyingSampling <- underlying.uniformSampling(samplingStart, samplingEnd, 1000)
      underlyingFiltered = underlyingSampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
      kldDensity <- KLDDensity(underlyingFiltered, utdSketch)
      kld <- KLD(underlyingFiltered, utdSketch)
    } yield (pdf, kldDensity, kld)
  }

}
