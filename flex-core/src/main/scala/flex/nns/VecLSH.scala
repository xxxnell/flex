package flex.nns

import cats.implicits._
import flex.pdf.UniformDist
import flex.rand.IRng
import flex.util.{EqAdapter, Memo}
import flex.util.Memo.syntax._
import flex.vec._

import scala.util.Try

trait VecLSH extends LSH[Vec] with VecLSHOps {

  val index: Int

}

trait VecLSHOps { lsh: VecLSH =>

  def mul(x: Vec): List[Float] =
    memo.get((EqAdapter(x), index), a.mmul(x).toFloatVector.toList)

  def shape: (Int, Int) = {
    val l = a.shape.headOption.getOrElse(0L).toInt
    val dim = Try(a.shape.apply(1)).getOrElse(0L).toInt

    (l, dim)
  }

}

object VecLSH {

  private case class VecLSHImpl(a: Vec, b: List[Float], w: List[Float], index: Int, memo: LSHMemo) extends VecLSH

  def apply(a: Vec, b: List[Float], w: List[Float], index: Int, memo: LSHMemo): VecLSH =
    VecLSHImpl(a, b, w, index, memo)

  def apply(dim: Int, w: List[Float], memoSize: Int, rng: IRng): (VecLSH, IRng) = {
    val l = w.size
    val (a, rng1) = Vec.std(dim * l, rng).leftMap(_.reshape(l, dim))
    val (b, rng2) = w.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    val memo = Memo.empty[(EqAdapter[Vec], Int), List[Float]](memoSize)
    (apply(a, b, w, 0, memo), rng2)
  }

  def fromSumVecLSH(lsh: SumVecLSH, i: Int): VecLSH = lsh.get(i)

}
