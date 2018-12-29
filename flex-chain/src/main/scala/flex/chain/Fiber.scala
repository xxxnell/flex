package flex.chain

import flex.pdf.VQH
import org.nd4j.linalg.api.ndarray.INDArray

trait Fiber extends Model {

  val inputVqh: VQH

  val outputVqh: VQH

}

trait FiberOps extends ModelOps {

  def map[A](model: Fiber, f: List[INDArray] => List[INDArray]): Fiber = ???

  def flatMap[A](model: Fiber, f: List[INDArray] => Fiber): Fiber = ???

  def update[A](s: Fiber, xob: INDArray, i: Int): Fiber = ???

  def input[A](mode: Fiber): VQH = ???

  def output[A](model: Fiber): VQH = ???

  def contract[A](model: Fiber): Fiber = ???

}

trait FiberSyntax {

  implicit class StreamSyntaxImpl(model: Fiber) {
    def map(f: List[INDArray] => List[INDArray]): Fiber = Fiber.map(model, f)
    def flatMap(f: List[INDArray] => Fiber): Fiber = Fiber.flatMap(model, f)
    def input: VQH = Fiber.input(model)
    def output: VQH = Fiber.output(model)
    def contract: Fiber = Fiber.contract(model)
    def add(means: INDArray, variances: INDArray): Fiber = ???
  }

}

object Fiber extends FiberOps {

  object syntax extends FiberSyntax

  private case class FiberImpl[A](inputVqh: VQH, outputVqh: VQH) extends Fiber

}