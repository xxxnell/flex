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

  override def toString: String = s"SumVecLSH(shape -> $shape)"

}

trait SumVecLSHOps { lsh: SumVecLSH =>

  // ops

  def muli(x: SumVec, i: Int): List[Float] = {
    val j = i + offset
    val xp = x.apply(j)
    val ap = a.map(_a => _a.apply(j))

    memo.get((EqAdapter(xp), j), ap.map(_ap => _ap.mmul(xp).getFloat(0l)))
  }

  def mul(x: SumVec): List[Float] = x.indices.foldLeft(List.fill(size)(0.0f)) {
    case (cum, i) => cum.zip(muli(x, i)).map { case (a1, a2) => a1 + a2 }
  }

  def dims: List[Int] = a.head.dims

  def shape: (Int, Int) = {
    val l = a.size
    val dim = dims.sum

    (l, dim)
  }

  def get(i: Int): VecLSH = VecLSH.fromSumVecLSH(lsh, i)

  def unzip: List[VecLSH] = dims.indices.toList.map(i => get(i))

}

object SumVecLSH {

  private case class SumVecLSHImpl(a: List[SumVec], b: List[Float], w: List[Float], memo: LSHMemo, offset: Int)
      extends SumVecLSH

  def apply(a: List[SumVec], b: List[Float], w: List[Float], memo: LSHMemo, offset: Int): SumVecLSH =
    SumVecLSHImpl(a, b, w, memo, offset)

  def apply(dims: List[Int], w: List[Float], memoSize: Int, rng: IRng): (SumVecLSH, IRng) = {
    val l = w.size
    val (a, rng1) = SumVec.stds(dims, rng, l).leftMap(_.map(_.zip(dims).map { case (v, dim) => v.reshape(1, dim) }))
    val (b, rng2) = w.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    val memo = Memo.empty[(EqAdapter[Vec], Int), List[Float]](memoSize)

    (apply(a, b, w, memo, 0), rng2)
  }

  def fromVecLSH(lsh: VecLSH): SumVecLSH = SumVecLSH(lsh.a.map(_a => SumVec(_a)), lsh.b, lsh.w, lsh.memo, lsh.offset)

}
