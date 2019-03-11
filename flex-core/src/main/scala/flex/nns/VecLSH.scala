package flex.nns

import flex.pdf.UniformDist
import flex.rand.IRng
import flex.vec._
import cats.implicits._

import scala.util.Try

trait VecLSHOps extends LSHOps[Vec] {

  def mul(lsh: LSH[Vec], x: Vec): Vec = lsh.a.mmul(x)

  def shape(lsh: LSH[Vec]): (Int, Int) = {
    val l = lsh.a.shape.headOption.getOrElse(0L).toInt
    val dim = Try(lsh.a.shape.apply(1)).getOrElse(0L).toInt

    (l, dim)
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
