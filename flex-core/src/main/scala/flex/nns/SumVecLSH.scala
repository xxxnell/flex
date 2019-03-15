package flex.nns

import cats.implicits._
import flex.pdf.UniformDist
import flex.rand._
import flex.util.{EqAdapter, Memo}
import flex.vec._
import flex.util.Memo.syntax._

import scala.util.Try

trait SumVecLSH extends LSH[SumVec] with SumVecLSHOps {

  val offset: Int

}

trait SumVecLSHOps { lsh: SumVecLSH =>

  // ops

  def muli(x: SumVec, i: Int): List[Float] = {
    val j = i + offset
    val xp = x.apply(j)
    memo.get((EqAdapter(xp), j), this.a.apply(j).mmul(xp).toFloatVector.toList)
  }

  def mul(x: SumVec): List[Float] = x.indices.foldLeft(List.fill(size)(0.0f)) {
    case (cum, i) => cum.zip(muli(x, i)).map { case (a1, a2) => a1 + a2 }
  }

  def dims: List[Int] = a.map(_a => Try(_a.shape.apply(1)).getOrElse(0L).toInt)

  def shape: (Int, Int) = {
    val l = a.headOption.flatMap(head => head.shape.headOption).getOrElse(0L).toInt
    val dim = dims.sum

    (l, dim)
  }

  def get(i: Int): VecLSH = VecLSH.fromSumVecLSH(lsh, i)

  def unzip: List[VecLSH] = dims.indices.toList.map(i => get(i))

}

object SumVecLSH {

  private case class SumVecLSHImpl(a: SumVec, b: List[Float], w: List[Float], memo: LSHMemo, offset: Int)
      extends SumVecLSH

  def apply(a: SumVec, b: List[Float], w: List[Float], memo: LSHMemo, offset: Int): SumVecLSH =
    SumVecLSHImpl(a, b, w, memo, offset)

  def apply(dims: List[Int], w: List[Float], memoSize: Int, rng: IRng): (SumVecLSH, IRng) = {
    val l = w.size
    val (a, rng1) = dims.foldRight((SumVec.empty, rng)) {
      case (dim, (_a, _rng)) => Vec.std(dim * l, _rng).leftMap(_.reshape(l, dim) :: _a)
    }
    val (b, rng2) = w.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    val memo = Memo.empty[(EqAdapter[Vec], Int), List[Float]](memoSize)

    (apply(a, b, w, memo, 0), rng2)
  }

  def fromVecLSH(lsh: VecLSH): SumVecLSH = SumVecLSH(SumVec(lsh.a), lsh.b, lsh.w, lsh.memo, lsh.offset)

}
