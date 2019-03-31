package flex.benchmark.complex

import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.vec._

object AlexNet {
  val (kin, kout) = (100, 10)

  val (l0, l1, l2, l3, l4, l5, l6, l7, l8) =
    (227 * 227, 55 * 55, 27 * 27, 13 * 13, 13 * 13, 256, 4096, 4096, 1)

  val (k0, k1, k2, k3, k4, k5, k6, k7, k8) =
    (20, 20, 20, 20, 20, 20, 20, 20, 20)

  def nn(v0: SumVec): SumVec =
    (((((((v0 match {
      case x1 :: z1 :: rem => z1.reshape(l1, l0).mmul(x1).tanh :: rem
    }) match {
      case h1 :: z2 :: rem => z2.reshape(l2, l1).mmul(h1).tanh :: rem
    }) match {
      case h2 :: z3 :: rem => z3.reshape(l3, l2).mmul(h2).tanh :: rem
    }) match {
      case h3 :: z4 :: rem => z4.reshape(l4, l3).mmul(h3).tanh :: rem
    }) match {
      case h4 :: z5 :: rem => z5.reshape(l5, l4).mmul(h4).tanh :: rem
    }) match {
      case h5 :: z6 :: rem => z6.reshape(l6, l5).mmul(h5).tanh :: rem
    }) match {
      case h6 :: z7 :: rem => z7.reshape(l7, l6).mmul(h6).tanh :: rem
    }) match {
      case h7 :: z8 :: rem => z8.reshape(l8, l7).mmul(h7).tanh :: rem
    }

  def complex: Complex =
    Complex
      .empty(kin, kout)
      .addStd(1 * l0 -> k0,
              l0 * l1 -> k1,
              l1 * l2 -> k2,
              l2 * l3 -> k3,
              l3 * l4 -> k4,
              l4 * l5 -> k5,
              l5 * l6 -> k6,
              l6 * l7 -> k7,
              l7 * l8 -> k8)
      .map(v0 => nn(v0))

}
