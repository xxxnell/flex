package flex.nns

import flex.pdf.UniformDist
import flex.rand._
import flex.vec._

trait SumVecLSHOps extends LSHOps[SumVec] {

  def hash(lsh: SumVecLSH, x: SumVec): Int =
    ((x.zip(lsh.a).map { case (_x, _a) => _a.mul(_x).getFloat(0) }.sum + lsh.b) / lsh.w).floor.round

  def dim(lsh: SumVecLSH): Int = lsh.a.map(_.dim).sum

}

object SumVecLSH extends SumVecLSHOps {

  private case class CodewordLSHImpl(a: SumVec, b: Float, w: Float) extends SumVecLSH

  def apply(a: SumVec, b: Float, w: Float): SumVecLSH = CodewordLSHImpl(a, b, w)

  def apply(dims: List[Int], w: Float, rng: IRng): (SumVecLSH, IRng) = {
    val (a, rng1) = SumVec.std(dims, rng)
    val (uniform, b) = UniformDist(w / 2, w / 2, rng1).sample
    (apply(a, b, w), uniform.rng)
  }

}
