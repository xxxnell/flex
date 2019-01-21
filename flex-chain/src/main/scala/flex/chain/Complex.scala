package flex.chain

import flex.pdf.VQH.syntax._
import flex.pdf._
import flex.vec._

trait Complex extends Model {

  val vqhin: VQH

  val vqhout: VQH

  val op: SumVec => SumVec

  val t: Map[SumVec, SumVec]

}

trait ComplexOps extends ModelOps with ComplexLaws {

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

trait ComplexLaws { self: ComplexOps =>

  def addStd(complex: Complex, dims: List[Int]): Complex = {
    val priorss = dims.map(dim => List.fill(dim)(NormalDist(0.0, 1.0)))
    priorss.foldLeft(complex) { case (_complex, priors) => addVar(_complex, priors) }
  }

}

trait ComplexSyntax {

  implicit class StreamSyntaxImpl(complex: Complex) {
    def map(f: SumVec => SumVec): Complex = Complex.map(complex, f)
    def addVar(priors: List[Dist[Double]]): Complex = Complex.addVar(complex, priors)
    def addStd(dims: List[Int]): Complex = Complex.addStd(complex, dims)
    def update(xps: List[(Vec, Int, Float)]): Complex = Complex.update(complex, xps)
    def update(xs: Vec*): Complex = Complex.update(complex, xs.toList.map(v => (v, 0, 1.0f)))
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
