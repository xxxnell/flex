package flex.nns

import cats.implicits._
import flex.pdf.UniformDist
import flex.rand.IRng
import flex.util.{ EqAdapter, Memo }
import flex.util.Memo.syntax._
import flex.vec._

import scala.util.Try

trait VecLSH extends LSH[Vec] with VecLSHOps {

  override def toString: String = s"VecLSH(shape -> $shape)"

}

trait VecLSHOps { lsh: VecLSH =>

  def mul(x: Vec): List[Float] =
    as.map(a => memo.get((EqAdapter(x), EqAdapter(a)), a.mmul(x).getFloat(0l)))

  def shape: (Int, Int) = {
    val l = as.size
    val dim = Try(as.head.dim).getOrElse(0)

    (l, dim)
  }

}

object VecLSH {

  private case class VecLSHImpl(as: List[Vec], bs: List[Float], ws: List[Float], memo: LSHMemo) extends VecLSH

  def apply(as: List[Vec], bs: List[Float], ws: List[Float], memo: LSHMemo): VecLSH =
    VecLSHImpl(as, bs, ws, memo)

  def apply(dim: Int, ws: List[Float], cache: Int, rng: IRng): (VecLSH, IRng) = {
    val l = ws.size
    val (as, rng1) = Vec.stds(dim, rng, l).leftMap(_.map(_.reshape(1, dim)))
    val (bs, rng2) = ws.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    val memo = LSHMemo.empty(cache * l)

    (apply(as, bs, ws, memo), rng2)
  }

  def fromSumVecLSH(lsh: SumVecLSH, i: Int): VecLSH =
    VecLSH(lsh.as.map(_a => _a.apply(i)), lsh.bs, lsh.ws, lsh.memo)

}
