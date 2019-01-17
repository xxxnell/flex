package flex.experiment.core

import flex.pdf.VQH
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.vec.SumVec

object MultivariateNormalExp {

  def main(args: Array[String]): Unit = {
    val expName1 = "multivariate-normal"
    val n = 100

    val (dims, k, rng) = (2 :: Nil, 20, IRng(0))
    val vqh0 = VQH.empty(dims, k)
    val (sumvecs, _) = SumVec.std(dims, rng, n)
    val vqhTr = vqh0 :: vqh0.expUpdateTrace(sumvecs.map((_, 1.0f)))
    val idxVqh = vqhTr.zipWithIndex.map(_.swap)

    idxVqh.foreach { case (i, vqh) => println(s"i: $i, vqh.size: ${vqh.size}") }
  }

}
