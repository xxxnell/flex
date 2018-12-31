package flex.chain

import flex.pdf.VQH
import flex.pdf.VQH.syntax._
import org.nd4j.linalg.api.ndarray.INDArray

trait Complex extends Model {

  val inputVqh: VQH

  val outputVqh: VQH

  val op: VQH#Codebook => VQH#Codebook

  val t: Map[VQH#Codebook, VQH#Codebook]

}

trait ComplexOps extends ModelOps {

  def map[A](fiber: Complex, f: VQH#Codebook => VQH#Codebook): Complex = {
    Complex(fiber.inputVqh, fiber.outputVqh, fiber.op.andThen(f), fiber.t)
  }

  def update[A](fiber: Complex, xs: List[(INDArray, Int, Float)]): Complex = {
    val (inputVqh1, cins, couts) = fiber.inputVqh.parUpdate(xs)
    val cys = cins.map(cin => (cin, fiber.op(cin)))
    val t1 = couts.foldLeft(fiber.t){ case (t, cout) => t.-(cout) }
    val t2 = cys.foldLeft(t1){ case (t, cy) => t.+(cy) }
    val ys = xs.flatMap { case (x1, i, w) => t2.get(inputVqh1.parSearch(x1, i)).map(y => (y, i, w)) }
    val outputVqh1 = fiber.outputVqh.expUpdate(ys)

    Complex(inputVqh1, outputVqh1, fiber.op, t2)
  }

}

trait ComplexSyntax {

  implicit class StreamSyntaxImpl(model: Complex) {
    def map(f: VQH#Codebook => VQH#Codebook): Complex = Complex.map(model, f)
    def add(means: INDArray, variances: INDArray): Complex = ???
  }

}

object Complex extends ComplexOps {

  object syntax extends ComplexSyntax

  private case class ComplexImpl[A](inputVqh: VQH,
                                    outputVqh: VQH,
                                    op: VQH#Codebook => VQH#Codebook,
                                    t: Map[VQH#Codebook, VQH#Codebook]) extends Complex

  def apply(inputVqh: VQH,
            outputVqh: VQH,
            op: VQH#Codebook => VQH#Codebook,
            t: Map[VQH#Codebook, VQH#Codebook]): Complex = ComplexImpl(inputVqh, outputVqh, op, t)

}