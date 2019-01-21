package flex.chain

import flex.pdf.{Dist, VQH}
import flex.vec._
import flex.pdf.VQH.syntax._
import org.nd4j.linalg.api.ndarray.INDArray

trait Complex extends Model {

  val vqhin: VQH

  val vqhout: VQH

  val op: SumVec => SumVec

  val t: Map[SumVec, SumVec]

}

trait ComplexOps extends ModelOps {

  def patchKin(complex: Complex, kin: Int): Complex = ???

  def patchKout(complex: Complex, kout: Int): Complex = ???

  def map[A](complex: Complex, f: SumVec => SumVec): Complex =
    Complex(complex.vqhin, complex.vqhout, complex.op.andThen(f), complex.t)

  def update[A](complex: Complex, xps: List[(Vec, Int, Float)]): Complex = {
    val (inputVqh1, cins, couts) = complex.vqhin.parUpdate(xps)
    val cys = cins.map(cin => (cin, complex.op(cin)))
    val t1 = complex.t -- couts ++ cys
    val ys = xps.flatMap { case (x1, i, w) => inputVqh1.parSearch(x1, i).flatMap(c => t1.get(c).map(y => (y, w))) }
    val (outputVqh1, _, _) = complex.vqhout.expUpdate(ys)

    Complex(inputVqh1, outputVqh1, complex.op, t1)
  }

  /**
   * Add independent variables to input space with prior distributions.
   * */
  def addVar(complex: Complex, priors: List[Dist[Double]]): Complex = {
    val vqhin1 = complex.vqhin.addDim(priors)
    val dims = complex.op(vqhin1.latest).dims
    val vqhout1 = VQH.empty(dims, complex.vqhout.k)
    val t1 = Map.empty[SumVec, SumVec]
    Complex(vqhin1, vqhout1, complex.op, t1)
  }

}

trait ComplexSyntax {

  implicit class StreamSyntaxImpl(complex: Complex) {
    def map(f: SumVec => SumVec): Complex = Complex.map(complex, f)
    def addVar(priors: List[Dist[Double]]): Complex = Complex.addVar(complex, priors)
    def add(mvs: List[(Double, Double)]): Complex = ???
    def addStd(dims: List[Int]): Complex = ???
  }

}

object Complex extends ComplexOps {

  object syntax extends ComplexSyntax

  private case class ComplexImpl[A](vqhin: VQH, vqhout: VQH, op: SumVec => SumVec, t: Map[SumVec, SumVec])
      extends Complex

  def apply(vqhin: VQH, vqhout: VQH, op: SumVec => SumVec, t: Map[SumVec, SumVec]): Complex =
    ComplexImpl(vqhin, vqhout, op, t)

  def empty(kin: Int, kout: Int): Complex =
    apply(VQH.empty(Nil, kin), VQH.empty(Nil, kout), identity, Map.empty[SumVec, SumVec])

}
