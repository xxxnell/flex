package flex.experiment.core

import flex.experiment.ops.ExpOutOps
import flex.pdf.VQH
import flex.pdf.VQH.syntax._
import flex.vec._
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._

object MNISTVQHExp {

  def main(args: Array[String]): Unit = {
    val expName = "mnist-vqh"
    val dataNo = 300
    val mnist = Dataset.mnistTrain.runSyncUnsafe(10.seconds)
    val ins = mnist.in.take(dataNo)
    val (dim, k) = (784, 20)
    val vqh0 = VQH.empty(dim, k)
    val vqhs = vqh0.expUpdateTrace(ins.map(v => (SumVec(v), 1.0f)))

    val instrs = ins.map(in => in.csv).mkString("\n")
    val vqhstrs = vqhs.zipWithIndex.map {
      case (vqh, i) => (i, vqh.cwns.map { case (sv, n) => sv.csv + "," + n / vqh.ntot }.mkString("\n"))
    }

    ExpOutOps.clear(expName)
    ExpOutOps.writeStr(expName, "in", instrs)
    ExpOutOps.writeStrs(expName, "vqh", vqhstrs)
  }

}
