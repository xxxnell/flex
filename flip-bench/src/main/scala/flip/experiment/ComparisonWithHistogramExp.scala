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
    val histoSampleNo = 20
    val sketchSampleNo = 20
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(sampleNo)

    val (histogram, histoConf) = self.histogram(histoSampleNo)
    val (sketch, sketchConf) = self.sketch(sketchSampleNo)

    val utdHistos = update(histogram, datas, 10, histoConf)
    val utdSketchs = update(sketch, datas, 10, sketchConf)

    val (histoResults, sketchResults) = (for {
      histoResults <- utdHistos.traverse { case (idx, utdHisto) =>
        results(utdHisto, datas, underlying, histoConf).map(result => (idx, result))
      }
      sketchResults <- utdSketchs.traverse { case (idx, utdSkt) =>
        results(utdSkt, datas, underlying, sketchConf).map(result => (idx, result))
      }
    } yield (histoResults, sketchResults))
      .getOrElse((Nil, Nil))

    ExpOutOps.clear(expName)
    ExpOutOps.writePlots(expName, "histo-pdf", histoResults.map { case (idx, (pdf, _, _)) => (idx, pdf) })
    ExpOutOps.writePlots(expName, "sketch-pdf", sketchResults.map { case (idx, (pdf, _, _)) => (idx, pdf) })
    ExpOutOps.writePlots(expName, "histo-kld-density", histoResults.map { case (idx, (_, kldd, _)) => (idx, kldd) })
    ExpOutOps.writePlots(expName, "sketch-kld-density", sketchResults.map { case (idx, (_, kldd, _)) => (idx, kldd) })
    ExpOutOps.writeStr(expName, "histo-kld", histoResults.map { case (idx, (_, _, kld)) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "sketch-kld", sketchResults.map { case (idx, (_, _, kld)) => s"$idx, $kld" }.mkString("\n"))
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
      queueSize = 0,
      cmapSize = no, cmapNo = 5, cmapStart = Some(-10d), cmapEnd = Some(10),
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
      underlyingSampling <- underlying.uniformSampling(samplingStart, samplingEnd, 1000)
      underlyingFiltered = underlyingSampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
      kldDensity <- KLDDensity(underlyingFiltered, sketch)(conf)
      kld <- KLD(underlyingFiltered, sketch)(conf)
    } yield (pdf, kldDensity, kld)
  }

}
