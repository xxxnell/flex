package flex.vec

import flex.rand._

trait SumVecOps {

  def dim(v: SumVec): Int = v.map(_v => _v.dim).sum

}

trait SumVecSyntax {

  implicit class SumVecSyntaxImpl(v: SumVec) {
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

  def zeros(dims: List[Int]): SumVec = dims.map(dim => Vec.zeros(dim))

}
