package flex.nns

import flex.pdf.UniformDist
import flex.rand.IRng
import flex.vec._
import cats.implicits._

trait VecLSHOps extends LSHOps[Vec] {

  def hashs(lsh: LSH[Vec], x: Vec): List[Int] =
    lsh.a.mmul(x).add(lsh.b).div(lsh.w).toFloatVector.toList.map(_.floor.round)

  def shape(lsh: LSH[Vec]): (Int, Int) = {
    val dims = lsh.a.shape.toList
    (dims.head.toInt, dims.tail.head.toInt)
  }

}

object VecLSH extends VecLSHOps {

  private case class VecLSHImpl(a: Vec, b: Vec, w: Vec) extends LSH[Vec]

  def apply(a: Vec, b: Vec, w: Vec): VecLSH = VecLSHImpl(a, b, w)

  def apply(dim: Int, w: List[Float], rng: IRng): (VecLSH, IRng) = {
    val l = w.size
    val (a, rng1) = Vec.std(dim * l, rng).leftMap(_.reshape(l, dim))
    val (b, rng2) = w.foldRight((List.empty[Float], rng1)) {
      case (_w, (_b, _rng)) => UniformDist.apply(_w / 2, _w / 2, _rng).sample.swap.bimap(s => s :: _b, d => d.rng)
    }
    (apply(a, Vec(b), Vec(w)), rng2)
  }

}
