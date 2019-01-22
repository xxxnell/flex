package flex.nns

import flex.pdf.UniformDist
import flex.rand.IRng
import flex.vec._
import cats.implicits._

trait VecLSHOps extends LSHOps[Vec] {

  def hash(lsh: LSH[Vec], x: Vec): Int = ((lsh.a.mul(x).getFloat(0) + lsh.b) / lsh.w).floor.round

  def dim(lsh: LSH[Vec]): Int = lsh.a.dim

}

object VecLSH extends VecLSHOps {

  private case class VecLSHImpl(a: Vec, b: Float, w: Float) extends LSH[Vec]

  def apply(a: Vec, b: Float, w: Float): VecLSH = VecLSHImpl(a, b, w)

  def apply(dim: Int, w: Float, rng: IRng): (VecLSH, IRng) = {
    val (a, rng1) = Vec.std(dim, rng).leftMap(_.reshape(dim, 1))
    val (uniform, b) = UniformDist(w / 2, w / 2, rng1).sample
    (apply(a, b, w), uniform.rng)
  }

}
