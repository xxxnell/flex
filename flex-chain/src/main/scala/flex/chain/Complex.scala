package flex.chain

import flex.pdf.VQH.syntax._
import flex.pdf._
import flex.vec._
import cats.implicits._

trait Complex extends Model {

  val in: VQH

  val pools: List[VQH]

  val out: VQH

  val op: SumVec => SumVec

  val t: Map[SumVec, SumVec]

}

trait ComplexOps extends ModelOps with ComplexLaws {

  def patchKin(complex: Complex, kin: Int): Complex = ???

  def patchKout(complex: Complex, kout: Int): Complex = ???

  def renewOut(complex: Complex): Complex = {
    val dims = complex.op(complex.in.last).dims
    val out1 = VQH.empty(dims, complex.out.k)
    val t1 = Map.empty[SumVec, SumVec]

    Complex(complex.in, complex.pools, out1, complex.op, t1)
  }

  def map[A](complex: Complex, f: SumVec => SumVec): Complex = {
    val f1 = complex.op.andThen(f)
    renewOut(Complex(complex.in, complex.pools, complex.out, f1, complex.t))
  }

  def update[A](complex: Complex, xps: List[(Vec, Int, Float)]): Complex = {
    val (xs, pools1) = complete(xps, complex.pools)
    val pools2 = updatePools(pools1, xps)
    val (in1, cins, couts) = complex.in.expUpdate(xs)
    val cys = cins.map(cin => (cin, complex.op(cin)))
    val t1 = complex.t -- couts ++ cys
    val ys = xps.flatMap { case (x1, i, w) => in1.parSearch(x1, i).flatMap(c => t1.get(c).map(y => (y, w))) }
    val (out1, _, _) = complex.out.expUpdate(ys)

    Complex(in1, pools2, out1, complex.op, t1)
  }

  private def complete(xps: List[(Vec, Int, Float)], pools: List[VQH]): (List[(SumVec, Float)], List[VQH]) = {
    val (xs, pj) = xps.foldRight((List.empty[(SumVec, Float)], pools.zipWithIndex)) {
      case ((xp, i, n), (xs0, pj0)) =>
        val (pj1, xx) = pj0.map { case (p0, j) => if (i == j) ((p0, j), SumVec(xp)) else p0.rand.leftMap((_, j)) }.unzip
        ((xx.map(_.head), n) :: xs0, pj1)
    }
    (xs, pj.unzip._1)
  }

  private def updatePools(pools: List[VQH], xps: List[(Vec, Int, Float)]): List[VQH] = {
    val pjs = xps.foldLeft(pools.zipWithIndex) {
      case (pjs0, (v, i, n)) =>
        pjs0.map { case (p, j) => (if (i == j) p.expUpdate((SumVec(v), n) :: Nil)._1 else p, j) }
    }
    pjs.unzip._1
  }

  /**
   * Add independent variables to input space with prior distributions.
   * */
  def addVar(complex: Complex, priors: List[Dist[Double]], k: Int): Complex = {
    val in1 = complex.in.addDim(priors)
    val pools1 = complex.pools.:+(VQH.empty(priors.size, k))
    renewOut(Complex(in1, pools1, complex.out, complex.op, complex.t))
  }

}

trait ComplexLaws { self: ComplexOps =>

  def addStd(complex: Complex, dimKs: List[(Int, Int)]): Complex = {
    val priorsKs = dimKs.map { case (dim, k) => (List.fill(dim)(NormalDist(0.0, 1.0)), k) }
    priorsKs.foldLeft(complex) { case (_complex, (priors, k)) => addVar(_complex, priors, k) }
  }

}

trait ComplexSyntax {

  implicit class StreamSyntaxImpl(complex: Complex) {
    def map(f: SumVec => SumVec): Complex = Complex.map(complex, f)
    def addVar(priors: List[Dist[Double]], k: Int): Complex = Complex.addVar(complex, priors, k)
    def addStd(dimKs: List[(Int, Int)]): Complex = Complex.addStd(complex, dimKs)
    def addStd(dimKs: (Int, Int)*): Complex = Complex.addStd(complex, dimKs.toList)
    def update(xps: List[(Vec, Int, Float)]): Complex = Complex.update(complex, xps)
    def update(xs: Vec*): Complex = Complex.update(complex, xs.toList.map(v => (v, 0, 1.0f)))
    def train(trainingset: Dataset): Complex = ???
    def evaluate(testset: Dataset): Float = ???
  }

}

object Complex extends ComplexOps {

  object syntax extends ComplexSyntax

  private case class ComplexImpl[A](in: VQH, pools: List[VQH], out: VQH, op: SumVec => SumVec, t: Map[SumVec, SumVec])
      extends Complex

  def apply(in: VQH, pools: List[VQH], out: VQH, op: SumVec => SumVec, t: Map[SumVec, SumVec]): Complex =
    ComplexImpl(in, pools, out, op, t)

  def empty(kin: Int, kout: Int): Complex =
    apply(VQH.empty(Nil, kin), Nil, VQH.empty(Nil, kout), identity, Map.empty[SumVec, SumVec])

}
