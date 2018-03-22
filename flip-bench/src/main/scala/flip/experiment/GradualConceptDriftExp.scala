package flip.experiment

import flip._
import flip.experiment.ops.ExpOutOps
import flip.rand.IRng

/**
  * A experiment for gradual concept drift.
  * https://edouardfouche.com/img/concept-drift/conceptdrift.png
  * */
object GradualConceptDriftExp {

  def main(args: Array[String]): Unit = {
    val expName = "gradual-cd-normal"
    val dataNo = 1000
    val draftStart = 300
    val draftStartingPoint = 0.0
    val velocity = 0.01
    val samplingNo = 20
    val start = 50
    val period = 100
    val domainWidth = 1.5

    def center(idx: Int) =
      if (draftStart > idx) draftStartingPoint
      else draftStartingPoint + velocity * (idx - draftStart)
    val rng: IRng = IRng(0)
    def underlying(idx: Int, rng: IRng = rng): NumericDist[Double] =
      if (draftStart > idx) NumericDist.normal(draftStartingPoint, 1.0, rng)
      else NumericDist.normal(center(idx), 1.0, rng)
    val datas: List[Double] = {
      var tempRng = rng
      (0 to dataNo).toList.map(idx => {
        val (utdDist, sample) = underlying(idx, tempRng).sample
        tempRng = utdDist.asInstanceOf[NumericDist[Double]].rng
        sample
      })
    }

    implicit val conf: SketchConf = SketchConf(
      startThreshold = start,
      thresholdPeriod = period,
      decayFactor = 1,
      queueSize = 30,
      cmapSize = samplingNo,
      cmapNo = 5,
      cmapStart = Some(-10d),
      cmapEnd = Some(10),
      boundaryRatio = 0.01,
      counterSize = samplingNo
    )
    val sketch0 = Sketch.empty[Double]
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxPdf = idxSketches.map { case (idx, skt) => (idx, skt.sampling) }
    val idxKld = idxSketches.map { case (idx, utdSkt) => (idx, KLD(underlying(idx), utdSkt)) }
    val idxCos = idxSketches.map { case (idx, utdSkt) => (idx, Cosine(underlying(idx), utdSkt)) }
    val idxEuc = idxSketches.map { case (idx, utdSkt) => (idx, Euclidean(underlying(idx), utdSkt)) }
    val idxSktMedian = idxSketches.map { case (idx, skt) => (idx, skt.median) }

    // out

    ExpOutOps.clear(expName)

    ExpOutOps.writePlots(expName, "pdf", idxPdf)
    ExpOutOps.writeStr(expName, "kld", idxKld.map { case (idx, kld) => s"$idx, $kld" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "cosine", idxCos.map { case (idx, cos) => s"$idx, $cos" }.mkString("\n"))
    ExpOutOps.writeStr(expName, "euclidean", idxEuc.map { case (idx, euc) => s"$idx, $euc" }.mkString("\n"))
    ExpOutOps.writeStr(
      expName,
      "median",
      idxSktMedian.map { case (idx, sktMed) => s"$idx, ${center(idx)}, $sktMed" }.mkString("\n"))

    // console print
    val avgSize = 10
    val avgKld = idxKld.takeRight(avgSize).map(_._2).sum / avgSize
    val avgCos = idxCos.takeRight(avgSize).map(_._2).sum / avgSize
    val avgEuc = idxEuc.takeRight(avgSize).map(_._2).sum / avgSize

    val str = s"Similarity for gradual concept-drifted data stream with velocity $velocity: \n" +
      s" KLD(Sketch($samplingNo)): $avgKld \n" +
      s" Cosine(Sketch($samplingNo)): $avgCos \n" +
      s" Euclidean(Sketch($samplingNo)): $avgEuc"
    println(str)
  }

}
