package flex.experiment.chain

import flex.vec._
import flex.chain.Complex
import flex.chain.Complex.syntax._
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

object MNISTExp {

  def main(args: Array[String]): Unit = {
    val mnistA = Dataset.mnistTrain.runSyncUnsafe(10.seconds)
    val mnistB = Dataset.mnistTest.runSyncUnsafe(10.seconds)
    val (kin, kout) = (20, 10)
    val (d0, l1, l2, l3) = (784, 10, 10, 1)
    val (k0, k1, k2, k3) = (20, 10, 10, 10)
    val model0 = Complex
      .empty(kin, kout)
      .addStd(d0 -> k0, d0 * l1 -> k1, l1 * l2 -> k2, l2 * l3 -> k3)
      .map { case x1 :: z1 :: rem => x1.mmul(z1.reshape(d0, l1)).tanh :: rem }
      .map { case h1 :: z2 :: rem => h1.mmul(z2.reshape(l1, l2)).tanh :: rem }
      .map { case h2 :: z3 :: rem => h2.mmul(z3.reshape(l2, l3)) :: rem }

    val model1 = model0.train(mnistA)
    val accuracy = model1.evaluate(mnistB)

    println(f"Accuracy on the MNIST: ${accuracy * 100}%1.1f%%")
  }

}
