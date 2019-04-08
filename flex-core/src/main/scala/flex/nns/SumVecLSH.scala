package flex.nns

import cats.implicits._
import flex.pdf.UniformDist
import flex.rand._
import flex.util.{EqAdapter, Memo}
import flex.vec._
import flex.util.Memo.syntax._

import scala.util.Try

trait SumVecLSH extends LSH[SumVec] with SumVecLSHOps {

  override def toString: String = s"SumVecLSH(shape -> $shape)"

}

trait SumVecLSHOps { lsh: SumVecLSH =>

  // ops

  def muli(x: SumVec, i: Int): List[Float] = {
    val xp = x.apply(i)
    val aps = as.map(a => a.apply(i))

    aps.map(ap => memo.get((EqAdapter(xp), EqAdapter(ap)), ap.mmul(xp).getFloat(0l)))
  }

  def mul(x: SumVec): List[Float] = x.indices.foldLeft(List.fill(size)(0.0f)) {
    case (cum, i) => cum.zip(muli(x, i)).map { case (a1, a2) => a1 + a2 }
  }

  def dims: List[Int] = as.head.dims

  def shape: (Int, Int) = {
    val l = as.size
    val dim = dims.sum

    (l, dim)
  }

  def get(i: Int): VecLSH = VecLSH.fromSumVecLSH(lsh, i)

  def unzip: List[VecLSH] = dims.indices.toList.map(i => get(i))

}

object SumVecLSH {

  private case class SumVecLSHImpl(as: List[SumVec], bs: List[Float], ws: List[Float], memo: LSHMemo) extends SumVecLSH

  def apply(as: List[SumVec], bs: List[Float], ws: List[Float], memo: LSHMemo): SumVecLSH =
    SumVecLSHImpl(as, bs, ws, memo)

  def apply(dims: List[Int], ws: List[Float], cache: Int, rng: IRng): (SumVecLSH, IRng) = {
    val l = ws.size
    val (as, rng1) = SumVec.stds(dims, rng, l).leftMap(_.map(_.zip(dims).map { case (v, dim) => v.reshape(1, dim) }))
    val (bs, rng2) = ws.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    val memo = LSHMemo.empty(cache * dims.size * l)

    (apply(as, bs, ws, memo), rng2)
  }

  def fromVecLSH(lsh: VecLSH): SumVecLSH = SumVecLSH(lsh.as.map(a => SumVec(a)), lsh.bs, lsh.ws, lsh.memo)

}
