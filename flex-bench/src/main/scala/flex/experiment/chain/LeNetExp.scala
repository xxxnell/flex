package flex.experiment.chain

import flex.LeNet
import flex.rand.IRng
import flex.vec._
import flex.pdf.VQH.syntax._
import flex.chain.Complex.syntax._

object LeNetExp {

  def main(args: Array[String]): Unit = {
    val (n, rng0) = (1000, IRng(0))
    val (complex, initTime) = flex.timeCost { LeNet.complex }
    val (dats, rng1) = Vec.stds(complex.in.dims.head, rng0, n)
    val (lat, rng2) = SumVec.std(complex.in.dims.tail, rng1)

    val (cnn, cnnTime) = flex.timeCost { dats.map(dat => LeNet.nn(dat :: lat)) }
    val (bnn, bnnTime) = flex.timeCost { complex.update(dats: _*) }

    println(
      s"LeNet initialization " +
      s"\n\tElapsed time: ${initTime / 1000000} ms" +
      s"\n\tParams number: ${LeNet.mln.numParams()}")
    println(
      s"LeNet(cnn):" +
      s"\n\tElapsed time: ${cnnTime / 1000000} ms (${flex.fmt(cnnTime.toDouble / 1000000 / n)} ms/data)")
    println(
      s"LeNet(bnn):" +
      s"\n\tresult: ${bnn.out}, " +
      s"\n\tElapsed time: ${bnnTime / 1000000} ms (${flex.fmt(bnnTime.toDouble / 1000000 / n)} ms/data)")
  }

}
