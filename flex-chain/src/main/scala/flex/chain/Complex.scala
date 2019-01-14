package flex.chain

import flex.pdf.{SumVec, VQH, Vec}
import flex.pdf.VQH.syntax._
import org.nd4j.linalg.api.ndarray.INDArray

trait Complex extends Model {

  val inputVqh: VQH

  val outputVqh: VQH

  val op: SumVec => SumVec

  val t: Map[SumVec, SumVec]

}

trait ComplexOps extends ModelOps {

  def map[A](complex: Complex, f: SumVec => SumVec): Complex =
    Complex(complex.inputVqh, complex.outputVqh, complex.op.andThen(f), complex.t)

  def update[A](complex: Complex, xps: List[(Vec, Int, Float)]): Complex = {
    val (inputVqh1, cins, couts) = complex.inputVqh.parUpdate(xps)
    val cys = cins.map(cin => (cin, complex.op(cin)))
    val t1 = complex.t -- couts ++ cys
    val ys = xps.flatMap { case (x1, i, w) => inputVqh1.parSearch(x1, i).flatMap(c => t1.get(c).map(y => (y, w))) }
    val (outputVqh1, _, _) = complex.outputVqh.expUpdate(ys)

    Complex(inputVqh1, outputVqh1, complex.op, t1)
  }

}

trait ComplexSyntax {

  implicit class StreamSyntaxImpl(model: Complex) {
    def map(f: SumVec => SumVec): Complex = Complex.map(model, f)
    def add(means: INDArray, variances: INDArray): Complex = ???
  }

}

object Complex extends ComplexOps {

  object syntax extends ComplexSyntax

  private case class ComplexImpl[A](inputVqh: VQH, outputVqh: VQH, op: SumVec => SumVec, t: Map[SumVec, SumVec])
      extends Complex

  def apply(inputVqh: VQH, outputVqh: VQH, op: SumVec => SumVec, t: Map[SumVec, SumVec]): Complex =
    ComplexImpl(inputVqh, outputVqh, op, t)

}
