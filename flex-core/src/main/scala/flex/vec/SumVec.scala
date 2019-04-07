package flex.vec

import flex.rand._
import cats.implicits._
import flex.util.EqAdapter
import org.nd4j.linalg.factory.Nd4j

trait SumVecOps {

  def dims(sv: SumVec): List[Int] = sv.map(_v => _v.dim)

  def dim(sv: SumVec): Int = dims(sv).sum

  def csv(sv: SumVec): String = sv.map(v => v.csv).mkString(",")

  def eqfy(sv: SumVec): List[EqAdapter[Vec]] = sv.map(v => EqAdapter(v))

  def concat(sv: SumVec): Vec = Nd4j.concat(1, sv.toArray: _*)

}

trait SumVecSyntax {

  implicit class SumVecSyntaxImpl(sv: SumVec) {
    def dims: List[Int] = SumVec.dims(sv)
    def dim: Int = SumVec.dim(sv)
    def csv: String = SumVec.csv(sv)
    def eqfy: List[EqAdapter[Vec]] = SumVec.eqfy(sv)
    def concat: Vec = SumVec.concat(sv)
  }

}

object SumVec extends SumVecOps {

  object syntax extends SumVecSyntax

  def apply(vs: Vec*): SumVec = vs.toList

  def apply(ass: List[List[Double]]): SumVec = ass.map(as => Vec(as))

  def empty: SumVec = Nil

  def std(dims: List[Int], rng: IRng): (SumVec, IRng) =
    dims.foldRight((List.empty[Vec], rng)) { case (dim, (sv, _rng)) => Vec.std(dim, _rng).leftMap(_ :: sv) }

  def stds(dimss: List[List[Int]], rng: IRng): (List[SumVec], IRng) = dimss.foldRight((List.empty[SumVec], rng)) {
    case (dims, (svs, _rng)) => std(dims, _rng).leftMap(_ :: svs)
  }

  def stds(dims: List[Int], rng: IRng, n: Int): (List[SumVec], IRng) = stds(List.fill(n)(dims), rng)

  def normal(paramss: List[List[(Float, Float)]], rng: IRng): (SumVec, IRng) =
    paramss.foldRight((List.empty[Vec], rng)) {
      case (params, (sv, _rng)) => Vec.normal(params, _rng).leftMap(v => v :: sv)
    }

  def zeros(dims: List[Int]): SumVec = dims.map(dim => Vec.zeros(dim))

}
