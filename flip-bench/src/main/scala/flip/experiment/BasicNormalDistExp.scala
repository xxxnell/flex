package flip.experiment

import flip.experiment.ops.ExpOutOps
import flip._
import flip.experiment.ops._

object BasicNormalDistExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "basic-normal"
    val dataNo = 1000
    val samplingNo = 100
    val start = 50
    val period = 100
    val minDomainCutoff = -10e10
    val maxDomainCutoff = 10e10

    implicit val conf: SketchConf = SketchConf(
      startThreshold = start, thresholdPeriod = period, queueSize = 30,
      cmapSize = samplingNo, cmapNo = 5, cmapStart = Some(-10d), cmapEnd = Some(10),
      counterSize = 1000, counterNo = 2
    )
    val sketch = Sketch.empty[Double]
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(dataNo)
    val dataIdxs = datas.zipWithIndex

    var tempSketchO: Option[Sketch[Double]] = Option(sketch)
    val idxUtdSketches: List[(Int, Sketch[Double])] = (0, sketch) :: dataIdxs.flatMap { case (data, idx) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO.map(tempSketch => (idx + 1, tempSketch))
    }.filter { case (idx, _) => idx % 10 == 0 }
    val idxDensityPlots = idxUtdSketches.flatMap { case (idx, utdSkt) => utdSkt.densityPlot.map(plot => (idx, plot)) }
    val idxKldDensity = idxUtdSketches.flatMap { case (idx, utdSkt) =>
      for {
        sampling <- underlying.sampling(utdSkt, conf)
        filtered = sampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
        plot <- KLDDensity(filtered, utdSkt)
      } yield (idx, plot)
    }
    val idxKld = idxUtdSketches.flatMap { case (idx, utdSkt) =>
      for {
        sampling <- underlying.sampling(utdSkt, conf)
        filtered = sampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
        kld <- KLD(filtered, utdSkt)
      } yield (idx, kld)
    }
    val idxCosineDensity = idxUtdSketches.flatMap { case (idx, utdSkt) =>
      for {
        sampling <- underlying.uniformSampling(-2.5, 2.5, samplingNo * 3)
        filtered = sampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
        cosine <- CosineDensity(filtered, utdSkt)
      } yield (idx, cosine)
    }
    val idsCosine = idxUtdSketches.flatMap { case (idx, utdSkt) =>
      for {
        sampling <- underlying.uniformSampling(-2.5, 2.5, samplingNo * 3)
        filtered = sampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
        cosine <- Cosine(filtered, utdSkt)
      } yield (idx, cosine)
    }

    ExpOutOps.clear(expName1)
    ExpOutOps.writePlots(expName1, "pdf", idxDensityPlots)
    ExpOutOps.writePlots(expName1, "kld-density", idxKldDensity)
    ExpOutOps.writeStr(expName1, "kld", idxKld.map{ case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writePlots(expName1, "cosine-density", idxCosineDensity)
    ExpOutOps.writeStr(expName1, "cosine", idsCosine.map{ case (idx, cosine) => s"$idx, $cosine" }.mkString("\n"))
  }

}
