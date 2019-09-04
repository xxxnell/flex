package flex.experiment.chain

import flex.AlexNet
import flex.rand.IRng
import flex.vec._
import flex.pdf.VQH.syntax._
import flex.chain.Complex.syntax._

object AlexNetExp {

  def main(args: Array[String]): Unit = {
    val (n, rng0) = (1000, IRng(0))
    val (complex, initTime) = flex.timeCost { AlexNet.complex }
    val (dats, rng1) = Vec.stds(complex.in.dims.head, rng0, n)
    val (lat, rng2) = SumVec.std(complex.in.dims.tail, rng1)

    val (cnn, cnnTime) = flex.timeCost { dats.map(dat => AlexNet.nn(dat :: lat)) }
    val (bnn, bnnTime) = flex.timeCost { complex.update(dats: _*) }

    println(
      s"AlexNet initialization " +
      s"\n\tElapsed time: ${initTime / 1000000} ms" +
      s"\n\tParams number: ${AlexNet.mln.numParams()}")
    println(
      s"AlexNet(cnn):" +
      f"\n\tElapsed time: ${cnnTime / 1000000} ms (${flex.fmt(cnnTime.toDouble / 1000000 / n)} ms/data)")
    println(
      s"AlexNet(bnn):" +
      s"\n\tresult: ${bnn.out}, " +
      f"\n\tElapsed time: ${bnnTime / 1000000} ms (${flex.fmt(bnnTime.toDouble / 1000000 / n)} ms/data)")
  }

}
