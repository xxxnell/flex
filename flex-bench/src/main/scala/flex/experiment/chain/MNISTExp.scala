package flex.experiment.chain

import flex.vec._
import flex.chain.Complex
import flex.chain.Complex.syntax._

object MNISTExp {

  def main(args: Array[String]): Unit = {
    val (kin, kout) = (20, 10)
    val (d0, l1, l2, l3) = (784, 10, 10, 1)
    val model = Complex
      .empty(kin, kout)
      .addStd(d0 :: d0 * l1 :: l1 * l2 :: l2 * l3 :: Nil)
      .map { case x1 :: z1 :: rem => x1.mmul(z1.reshape(d0, l1)).tanh :: rem }
      .map { case h1 :: z2 :: rem => h1.mmul(z2.reshape(l1, l2)).tanh :: rem }
      .map { case h2 :: z3 :: rem => h2.mmul(z3.reshape(l2, l3)) :: rem }
  }

}
