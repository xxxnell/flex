package flip.experiment

import flip.Params
import flip.conf.pdf.AdaSelSketchConf
import flip.experiment.ops._
import flip.implicits._
import flip.rand.IRng

object ConfExp {

  def main(args: Array[String]): Unit = {
    val expName = "conf"

    val confs: List[SketchConf] = Params.default.toConfs
    val normal = (idx: Int) => NumericDist.normal(0.0, 1, IRng(idx))
    val incr = (idx: Int) => NumericDist.normal(idx * 0.01, 1, IRng(idx))
    val normalMeanKlds: List[(SketchConf, Double)] = confs.map(conf => (conf, meanKld(conf, normal)))
    val incrDriftMeanKlds: List[(SketchConf, Double)] = confs.map(conf => (conf, meanKld(conf, incr)))

    val confHeader = "cmapSize, cmapNo, bufferSize, decayFactor, rebuildThreshold, kld\n"
    val conf2Str: SketchConf => String = conf =>
      s"${conf.cmap.size}, " +
        s"${conf.cmap.no}, " +
        s"${conf.asInstanceOf[AdaSelSketchConf].bufferSize}, " +
        s"${conf.decayFactor}, " +
        s"${conf.asInstanceOf[AdaSelSketchConf].rebuildThreshold}"
    val normalOut = confHeader +
      normalMeanKlds.map { case (conf, kld) => conf2Str(conf) + s", $kld" }.mkString("\n")
    val incrDriftOut = confHeader +
      incrDriftMeanKlds.map { case (conf, kld) => conf2Str(conf) + s", $kld" }.mkString("\n")

    ExpOutOps.clear(expName)
    ExpOutOps.writeStr(expName, "normal", normalOut)
    ExpOutOps.writeStr(expName, "incr-drift", incrDriftOut)
  }

  def meanKld(conf: SketchConf, underlying: Int => Dist[Double]): Double = {
    val dataNo = 1000
    val measureStart = 600
    val measureEnd = 1000

    implicit val confImpl: SketchConf = conf
    val sketch0 = Sketch.empty[Double]
    val datas = (1 to dataNo).toList.map(idx => underlying(idx).sample._2)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxKld = idxSketches.map { case (idx, sketch) => (idx, math.abs(KLD(underlying(idx), sketch))) }

    mean(idxKld.filter { case (idx, _) => idx >= measureStart && idx <= measureEnd }.unzip._2)
  }

}
