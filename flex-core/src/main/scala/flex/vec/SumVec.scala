package flex.vec

import flex.pdf.Dist
import flex.rand._

trait SumVecOps {

  def dims(v: SumVec): List[Int] = v.map(_v => _v.dim)

  def dim(v: SumVec): Int = dims(v).sum

}

trait SumVecSyntax {

  implicit class SumVecSyntaxImpl(v: SumVec) {
    def dims: List[Int] = SumVec.dims(v)
    def dim: Int = SumVec.dim(v)
  }

}

object SumVec extends SumVecOps {

  object syntax extends SumVecSyntax

  def apply(ass: List[List[Double]]): SumVec = ass.map(as => Vec(as))

  def std(dims: List[Int], rng: IRng): (SumVec, IRng) = dims.foldRight((List.empty[Vec], rng)) {
    case (dim, (_sumVec, rng1)) =>
      val (vec, rng2) = Vec.std(dim, rng1)
      (vec :: _sumVec, rng2)
  }

  def std(dims: List[Int], rng: IRng, n: Int): (List[SumVec], IRng) =
    (1 to n).foldRight((List.empty[SumVec], rng)) {
      case (i, (sumvecs, _rng1)) =>
        val (sumvec, _rng2) = std(dims, _rng1)
        (sumvec :: sumvecs, _rng2)
    }

  def zeros(dims: List[Int]): SumVec = dims.map(dim => Vec.zeros(dim))

}
