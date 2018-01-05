package sketch.scope.experiment

import sketch.scope._
import sketch.scope.experiment.ops.ExpOutOps

/**
  * A experiment for sudden concept drift.
  * https://edouardfouche.com/img/concept-drift/conceptdrift.png
  * */
object SuddenConceptDriftExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "sudden-cd-normal"
    val sampleNo1 = 1000
    val sampleNo2 = 1000
    val start = 50
    val period = 100
    val minDomainCutoff = -10e10
    val maxDomainCutoff = 10e10

    implicit val conf: SketchConf = SketchConf(
      startThreshold = start, thresholdPeriod = period, queueSize = 30,
      cmapSize = 150, cmapNo = 5, cmapStart = Some(-10d), cmapEnd = Some(10),
      counterSize = 1000, counterNo = 2
    )
    val sketch = Sketch.empty[Double]
    val underlying1 = Dist.normal(0.1, 1)
    val (_, datas1) = underlying1.samples(sampleNo1)
    val underlying2 = Dist.normal(5.1, 1)
    val (_, datas2) = underlying2.samples(sampleNo2)
    val dataIdxs = (datas1 ++ datas2).zipWithIndex

    var tempSketchO: Option[Sketch[Double]] = Option(sketch)
    val idxUtdSketches: List[(Int, Sketch[Double])] = (0, sketch) :: dataIdxs.flatMap { case (data, idx) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO.map(tempSketch => (idx + 1, tempSketch))
    }.filter { case (idx, _) => (idx - start) % period == 0 || (idx - start + 1) % period == 0 }
    val idxDensityPlots = idxUtdSketches.flatMap { case (idx, utdSkt) => utdSkt.densityPlot.map(plot => (idx, plot)) }
    val idxKldPlot = idxUtdSketches.flatMap { case (idx, utdSkt) =>
      for {
        sampling <- if(idx < sampleNo1) underlying1.sampling(utdSkt) else underlying2.sampling(utdSkt)
        filtered = sampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
        plot <- KLDDensity(filtered, utdSkt)
      } yield (idx, plot)
    }
    val idxKld = idxUtdSketches.flatMap { case (idx, utdSkt) =>
      for {
        sampling <- if(idx < sampleNo1) underlying1.sampling(utdSkt) else underlying2.sampling(utdSkt)
        filtered = sampling.filter { range => range > minDomainCutoff && range < maxDomainCutoff }
        kld <- KLD(filtered, utdSkt)
      } yield (idx, kld)
    }

    ExpOutOps.clear(expName1)
    ExpOutOps.writePlots(expName1, idxDensityPlots)
    ExpOutOps.writePlots(expName1, "kld", idxKldPlot)
    println(s"KLD Trend for $expName1: $idxKld")
  }

}
