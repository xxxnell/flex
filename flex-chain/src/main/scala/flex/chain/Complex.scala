package flex.chain

import flex.pdf.VQH.syntax._
import flex.pdf._
import flex.vec._
import cats.implicits._
import flex.rand.IRng

trait Complex extends Model {

  val in: VQH

  val pools: List[VQH]

  val out: VQH

  val op: SumVec => SumVec

  val t: Map[SumVec, SumVec]

}

trait ComplexOps extends ModelOps with ComplexLaws {

  def patchKin(complex: Complex, kin: Int): Complex =
    Complex(complex.in.patchK(kin), complex.pools, complex.out, complex.op, complex.t)

  def patchKout(complex: Complex, kout: Int): Complex =
    Complex(complex.in.patchK(kout), complex.pools, complex.out, complex.op, complex.t)

  /**
   * @param ks list of k for pools
   * */
  def patchIn(complex: Complex, in: VQH, ks: List[Int]): Complex = {
    val pools1 = in.unzip.zip(ks).map { case (vqh, k) => vqh.patchK(k) }

    renewOut(Complex(in, pools1, complex.out, complex.op, complex.t))
  }

  /**
   * @param k k for in
   * */
  def patchPools(complex: Complex, pools: List[VQH], k: Int): Complex = {
    val dims = pools.flatMap(pool => pool.dims)
    val in1 = VQH.empty(dims, k)

    renewOut(Complex(in1, pools, complex.out, complex.op, complex.t))
  }

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

  def update[A](complex: Complex, xpn: (Vec, Int, Float)): Complex = {
    val (x, i, n) = xpn
    val pools1 = complex.pools.zipWithIndex.map {
      case (p, j) => if (i == j) p.expUpdate((SumVec(x), n) :: Nil)._1 else p
    }
    val (xs, _, pools2) = complete(xpn, pools1)
    val (in1, cins, couts) = complex.in.parUpdate(xpn :: Nil, { case (_: Vec, _: Int, _: SumVec) => xs })
    val cys = cins.map(cin => (cin, complex.op(cin)))
    val t1 = complex.t -- couts ++ cys
    val yn = in1.parSearch(x, i).flatMap(c => t1.get(c).map(y => (y, n)))
    val (out1, _, _) = complex.out.expUpdate(yn.toList)

    Complex(in1, pools2, out1, complex.op, t1)
  }

  private def complete(xp: (Vec, Int, Float), pools: List[VQH]): (SumVec, Float, List[VQH]) = {
    val (x, i, n) = xp
    val (pools1, xs) = pools.zipWithIndex.map { case (p, j) => if (i != j) p.rand.map(_.head) else (p, x) }.unzip
    (xs, n, pools1)
  }

  def updates[A](complex: Complex, xpns: List[(Vec, Int, Float)]): Complex =
    xpns.foldLeft(complex) { case (_complex, xpn) => update(_complex, xpn) }

  def clear(complex: Complex): Unit = {
    complex.in.clear
    complex.pools.foreach(_.clear)
    complex.out.clear
  }

}

trait ComplexLaws { self: ComplexOps =>

  def addDimStd(complex: Complex, dimKs: List[(Int, Int)]): Complex = {
    val (dims, ks) = dimKs.unzip
    patchIn(complex, complex.in.addDimStd(dims), complex.pools.map(_.k) ++ ks)
  }

  def initNormal(complex: Complex, paramss: List[List[(Float, Float)]]): Complex =
    patchPools(
      complex,
      complex.pools.zip(paramss).map { case (pool, params) => pool.initNormal(params :: Nil) },
      complex.in.k
    )

  def initStd(complex: Complex): Complex = {
    val params = complex.in.dims.map(dim => List.fill(dim)((0.0f, 1.0f)))
    initNormal(complex, params)
  }

}

trait ComplexSyntax {

  implicit class ComplexSyntaxImpl(complex: Complex) {
    def map(f: SumVec => SumVec): Complex = Complex.map(complex, f)
    def addDim(dimKs: List[(Int, Int)]): Complex = Complex.addDimStd(complex, dimKs)
    def addDim(dimKs: (Int, Int)*): Complex = Complex.addDimStd(complex, dimKs.toList)
    def addDimStd(dimKs: List[(Int, Int)]): Complex = Complex.addDimStd(complex, dimKs)
    def addDimStd(dimKs: (Int, Int)*): Complex = Complex.addDimStd(complex, dimKs.toList)
    def updates(xps: List[(Vec, Int, Float)]): Complex = Complex.updates(complex, xps)
    def update(xs: Vec*): Complex = Complex.updates(complex, xs.toList.map(v => (v, 0, 1.0f)))
    def train(trainingset: Dataset): Complex = ???
    def evaluate(testset: Dataset): Float = ???
    def clear: Unit = Complex.clear(complex)
    def init: Complex = Complex.initStd(complex)
    def initNormal(params: List[List[(Float, Float)]]): Complex = Complex.initNormal(complex, params)
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
