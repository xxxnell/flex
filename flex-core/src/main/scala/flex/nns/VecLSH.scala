package flex.nns

import flex.pdf.{NormalDist, UniformDist, Vec}
import flex.rand.IRng
import org.nd4j.linalg.factory.Nd4j

trait VecLSHOps extends LSHOps[Vec] {

  def hash(lsh: LSH[Vec], x: Vec): Int = ((lsh.a.mul(x).getFloat(0) + lsh.b) / lsh.w).floor.round

  def dim(lsh: LSH[Vec]): Int = lsh.a.shape.apply(1).toInt

}

object VecLSH extends VecLSHOps {

  private case class VecLSHImpl(a: Vec, b: Float, w: Float) extends LSH[Vec]

  def apply(a: Vec, b: Float, w: Float): VecLSH = VecLSHImpl(a, b, w)

  def apply(dim: Int, w: Float, rng: IRng): (VecLSH, IRng) = {
    val (normal, af) = NormalDist(0.0, 1.0, rng).samples(dim)
    val a = Nd4j.create(af.toArray)
    val (uniform, b) = UniformDist(w / 2, w / 2, normal.rng).sample
    (apply(a, b, w), uniform.rng)
  }

}
