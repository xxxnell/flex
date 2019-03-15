package flex.vec

import flex.rand._
import cats.implicits._
import flex.util.EqAdapter

trait SumVecOps {

  def dims(sv: SumVec): List[Int] = sv.map(_v => _v.dim)

  def dim(sv: SumVec): Int = dims(sv).sum

  def csv(sv: SumVec): String = sv.map(v => v.csv).mkString(",")

  def eqfy(sv: SumVec): List[EqAdapter[Vec]] = sv.map(v => EqAdapter(v))

}

trait SumVecSyntax {

  implicit class SumVecSyntaxImpl(sv: SumVec) {
    def dims: List[Int] = SumVec.dims(sv)
    def dim: Int = SumVec.dim(sv)
    def csv: String = SumVec.csv(sv)
    def eqfy: List[EqAdapter[Vec]] = SumVec.eqfy(sv)
  }

}

object SumVec extends SumVecOps {

  object syntax extends SumVecSyntax

  def apply(vs: Vec*): SumVec = vs.toList

  def apply(ass: List[List[Double]]): SumVec = ass.map(as => Vec(as))

  def empty: SumVec = Nil

  def std(dims: List[Int], rng: IRng): (SumVec, IRng) =
    dims.foldRight((List.empty[Vec], rng)) { case (dim, (sv, _rng)) => Vec.std(dim, _rng).leftMap(_ :: sv) }

  def stds(dims: List[Int], rng: IRng, n: Int): (List[SumVec], IRng) =
    (0 until n).foldRight((List.empty[SumVec], rng)) { case (_, (svs, _rng)) => std(dims, _rng).leftMap(_ :: svs) }

  def zeros(dims: List[Int]): SumVec = dims.map(dim => Vec.zeros(dim))

}
