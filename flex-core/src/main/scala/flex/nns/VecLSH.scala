package flex.nns

import cats.implicits._
import flex.pdf.UniformDist
import flex.rand.IRng
import flex.util.{EqAdapter, Memo}
import flex.util.Memo.syntax._
import flex.vec._

import scala.util.Try

trait VecLSH extends LSH[Vec] with VecLSHOps {

  val offset: Int

  override def toString: String = s"VecLSH(shape -> $shape)"

}

trait VecLSHOps { lsh: VecLSH =>

  def mul(x: Vec): List[Float] =
    memo.get((EqAdapter(x), offset), a.map(_a => _a.mmul(x).getFloat(0l)))

  def shape: (Int, Int) = {
    val l = a.size
    val dim = Try(a.head.dim).getOrElse(0)

    (l, dim)
  }

}

object VecLSH {

  private case class VecLSHImpl(a: List[Vec], b: List[Float], w: List[Float], offset: Int, memo: LSHMemo) extends VecLSH

  def apply(a: List[Vec], b: List[Float], w: List[Float], index: Int, memo: LSHMemo): VecLSH =
    VecLSHImpl(a, b, w, index, memo)

  def apply(dim: Int, w: List[Float], memoSize: Int, rng: IRng): (VecLSH, IRng) = {
    val l = w.size
    val (a, rng1) = Vec.stds(dim, rng, l).leftMap(_.map(_.reshape(1, dim)))
    val (b, rng2) = w.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    val memo = Memo.empty[(EqAdapter[Vec], Int), List[Float]](memoSize)

    (apply(a, b, w, 0, memo), rng2)
  }

  def fromSumVecLSH(lsh: SumVecLSH, i: Int): VecLSH =
    VecLSH(lsh.a.map(_a => _a.apply(i)), lsh.b, lsh.w, lsh.offset + i, lsh.memo)

}
