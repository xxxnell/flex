package flex.benchmark.complex

import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.vec._

object FCMNISTNet {

  val (kin, kout) = (20, 10)

  val (l0, l1, l2, l3) = (28 * 28, 10, 10, 1)

  val (k0, k1, k2, k3) = (20, 20, 20, 20)

  def nn(v0: SumVec): SumVec =
    ((v0 match {
      case x1 :: z1 :: rem => z1.reshape(l1, l0).mmul(x1).tanh :: rem
    }) match {
      case h1 :: z2 :: rem => z2.reshape(l2, l1).mmul(h1).tanh :: rem
    }) match {
      case h2 :: z3 :: rem => z3.reshape(l3, l2).mmul(h2) :: rem
    }

  def complex: Complex =
    Complex
      .empty(kin, kout)
      .addStd(l0 -> k0, l0 * l1 -> k1, l1 * l2 -> k2, l2 * l3 -> k3)
      .map(v0 => nn(v0))

}
