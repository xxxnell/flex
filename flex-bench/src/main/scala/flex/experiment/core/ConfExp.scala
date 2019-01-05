package flex.experiment.core

import flex.conf.pdf.AdaSelSketchConf
import flex.experiment.ops._
import flex.implicits._
import flex.rand.IRng
import flex.{Params, Profiler}

object ConfExp {

  def main(args: Array[String]): Unit = {
    val expName = "conf"

    val confs: List[SketchConf] = Params.default.toConfs
    val normal = (idx: Int) => NumericDist.normal(0.0, 1, IRng(idx))
    val incr = (idx: Int) => NumericDist.normal(idx * 0.01, 1, IRng(idx))
    val normalEds: Map[SketchConf, Double] = confs.map(conf => (conf, meanEd(conf, normal))).toMap
    val driftEds: Map[SketchConf, Double] = confs.map(conf => (conf, meanEd(conf, incr))).toMap
    val mems: Map[SketchConf, Long] = confs.map(conf => (conf, mem(conf, incr))).toMap
    println("(1/2) Calculation is completed...")

    val confHeader = "cmapSize, cmapNo, bufferSize, decayFactor, rebuildThreshold, ed, mem\n"
    val conf2Str: SketchConf => String = conf =>
      s"${conf.cmap.size}, " +
        s"${conf.cmap.no}, " +
        s"${conf.asInstanceOf[AdaSelSketchConf].bufferSize}, " +
        s"${conf.decayFactor}, " +
        s"${conf.asInstanceOf[AdaSelSketchConf].rebuildThreshold}"
    val normalOut =
      confHeader +
        confs
          .map { conf =>
            s"${conf2Str(conf)}, ${normalEds.getOrElse(conf, "")}, ${mems.getOrElse(conf, "")}"
          }
          .mkString("\n")
    val incrDriftOut =
      confHeader +
        confs
          .map { conf =>
            s"${conf2Str(conf)}, ${driftEds.getOrElse(conf, "")}, ${mems.getOrElse(conf, "")}"
          }
          .mkString("\n")
    println("(2/2) Results are printed...")

    ExpOutOps.clear(expName)
    ExpOutOps.writeStr(expName, "normal", normalOut)
    ExpOutOps.writeStr(expName, "incr-drift", incrDriftOut)
  }

  def meanEd(conf: SketchConf, underlying: Int => Dist[Double]): Double = {
    val dataNo = 1000
    val measureStart = 600
    val measureEnd = 1000

    implicit val confImpl: SketchConf = conf
    val sketch0 = Sketch.empty[Double]
    val datas = (1 to dataNo).toList.map(idx => underlying(idx).sample._2)
    val sketchTraces = sketch0 :: sketch0.updateTrace(datas)
    val idxSketches = sketchTraces.indices.zip(sketchTraces).toList.filter { case (idx, _) => idx % 10 == 0 }
    val idxEd = idxSketches.map { case (idx, sketch)                                       => (idx, math.abs(ED(underlying(idx), sketch))) }

    mean(idxEd.filter { case (idx, _) => idx >= measureStart && idx <= measureEnd }.unzip._2)
  }

  def mem(conf: SketchConf, underlying: Int => Dist[Double]): Long = {
    val dataNo = 1000

    implicit val confImpl: SketchConf = conf
    val sketch0 = Sketch.empty[Double]
    val datas = (1 to dataNo).toList.map(idx => underlying(idx).sample._2)
    val sketch1 = (1 to conf.cmap.no).map(_ => sketch0.updateInOrder(datas).rebuild).last

    Profiler.serializedMem(sketch1)
  }

}
