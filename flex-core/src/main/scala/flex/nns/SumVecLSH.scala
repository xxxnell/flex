package flex.nns

import flex.pdf.UniformDist
import flex.rand._
import flex.vec._
import cats.implicits._

trait SumVecLSHOps extends LSHOps[SumVec] {

  def hashs(lsh: SumVecLSH, x: SumVec): List[Int] = {
    val m = lsh.a.zip(x).map { case (ap, xp) => ap.mmul(xp) }.foldLeft(Vec.zeros(size(lsh))) {
      case (acc, _m) => acc.add(_m)
    }
    m.add(lsh.b).div(lsh.w).toFloatVector.toList.map(_.floor.round)
  }

  def shape(lsh: SumVecLSH): (Int, Int) = (lsh.a.head.shape.head.toInt, lsh.a.map(_a => _a.shape.tail.head).sum.toInt)

}

object SumVecLSH extends SumVecLSHOps {

  private case class CodewordLSHImpl(a: SumVec, b: Vec, w: Vec) extends SumVecLSH

  def apply(a: SumVec, b: Vec, w: Vec): SumVecLSH = CodewordLSHImpl(a, b, w)

  def apply(dims: List[Int], w: List[Float], rng: IRng): (SumVecLSH, IRng) = {
    val l = w.size
    val (a, rng1) = dims.foldRight((SumVec.empty, rng)) {
      case (dim, (_a, _rng)) => Vec.std(dim * l, _rng).leftMap(_.reshape(l, dim) :: _a)
    }
    val (b, rng2) = w.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    (apply(a, Vec(b), Vec(w)), rng2)
  }

}
