package flex.chain

import flex.pdf.VQH
import flex.pdf.VQH.syntax._
import org.nd4j.linalg.api.ndarray.INDArray

trait Complex extends Model {

  val inputVqh: VQH

  val outputVqh: VQH

  val op: VQH#Codeword => VQH#Codeword

  val t: Map[VQH#Codeword, VQH#Codeword]

}

trait ComplexOps extends ModelOps {

  def map[A](fiber: Complex, f: VQH#Codeword => VQH#Codeword): Complex =
    Complex(fiber.inputVqh, fiber.outputVqh, fiber.op.andThen(f), fiber.t)

  def update[A](fiber: Complex, xs: List[(INDArray, Int, Float)]): Complex = {
    val (inputVqh1, cins, couts) = fiber.inputVqh.parUpdate(xs)
    val cys = cins.map(cin => (cin, fiber.op(cin)))
    val t1 = fiber.t -- couts ++ cys
    val ys = xs.flatMap { case (x1, i, w) => inputVqh1.parSearch(x1, i).flatMap(c => t1.get(c).map(y => (y, w))) }
    val (outputVqh1, _, _) = fiber.outputVqh.expUpdate(ys)

    Complex(inputVqh1, outputVqh1, fiber.op, t1)
  }

}

trait ComplexSyntax {

  implicit class StreamSyntaxImpl(model: Complex) {
    def map(f: VQH#Codeword => VQH#Codeword): Complex = Complex.map(model, f)
    def add(means: INDArray, variances: INDArray): Complex = ???
  }

}

object Complex extends ComplexOps {

  object syntax extends ComplexSyntax

  private case class ComplexImpl[A](inputVqh: VQH,
                                    outputVqh: VQH,
                                    op: VQH#Codeword => VQH#Codeword,
                                    t: Map[VQH#Codeword, VQH#Codeword])
      extends Complex

  def apply(inputVqh: VQH,
            outputVqh: VQH,
            op: VQH#Codeword => VQH#Codeword,
            t: Map[VQH#Codeword, VQH#Codeword]): Complex = ComplexImpl(inputVqh, outputVqh, op, t)

}
