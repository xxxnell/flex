package flex.nns

import cats.implicits._
import flex.pdf.UniformDist
import flex.rand._
import flex.util.{EqAdapter, Memo}
import flex.vec._
import flex.util.Memo.syntax._

import scala.util.Try

trait SumVecLSH extends LSH[SumVec] with SumVecLSHOps

trait SumVecLSHOps { lsh: SumVecLSH =>

  // ops

  def muli(x: SumVec, i: Int): List[Float] = {
    val xp = x.apply(i)
    memo.get((EqAdapter(xp), i), this.a.apply(i).mmul(xp).toFloatVector.toList)
  }

  def mul(x: SumVec): List[Float] = x.indices.foldLeft(List.fill(size)(0.0f)) {
    case (cum, i) => cum.zip(muli(x, i)).map { case (a1, a2) => a1 + a2 }
  }

  def shape: (Int, Int) = {
    val l = a.headOption.flatMap(head => head.shape.headOption).getOrElse(0L).toInt
    val dim = a.map(_a => Try(_a.shape.apply(1)).getOrElse(0L)).sum.toInt

    (l, dim)
  }

  def toVecLSH(i: Int): VecLSH = VecLSH(a.apply(i), b, w, i, memo)

}

object SumVecLSH {

  private case class SumVecLSHImpl(a: SumVec, b: List[Float], w: List[Float], memo: LSHMemo) extends SumVecLSH

  def apply(a: SumVec, b: List[Float], w: List[Float], memo: LSHMemo): SumVecLSH = SumVecLSHImpl(a, b, w, memo)

  def apply(dims: List[Int], w: List[Float], memoSize: Int, rng: IRng): (SumVecLSH, IRng) = {
    val l = w.size
    val (a, rng1) = dims.foldRight((SumVec.empty, rng)) {
      case (dim, (_a, _rng)) => Vec.std(dim * l, _rng).leftMap(_.reshape(l, dim) :: _a)
    }
    val (b, rng2) = w.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    val memo = Memo.empty[(EqAdapter[Vec], Int), List[Float]](memoSize)
    (apply(a, b, w, memo), rng2)
  }

}
