package flex.nns

import flex.pdf.{Dist, NormalDist, SumVec, UniformDist, VQH}
import flex.rand.IRng
import org.nd4j.linalg.factory.Nd4j

trait SumVecLSHOps extends LSHOps[SumVec] {

  def hash(lsh: SumVecLSH, x: SumVec): Int =
    ((x.zip(lsh.a).map { case (_x, _a) => _a.mul(_x).getFloat(0) }.sum + lsh.b) / lsh.w).floor.round

  def dim(lsh: SumVecLSH): Int = lsh.a.map(_.shape.apply(1)).sum.toInt

}

object SumVecLSH extends SumVecLSHOps {

  private case class CodewordLSHImpl(a: SumVec, b: Float, w: Float) extends SumVecLSH

  def apply(a: SumVec, b: Float, w: Float): SumVecLSH = CodewordLSHImpl(a, b, w)

  def apply(dims: List[Int], w: Float, rng: IRng): (SumVecLSH, IRng) = {
    val (normal, afs) = dims.foldRight[(Dist[Double], List[List[Double]])]((NormalDist(0.0, 1.0, rng), Nil)) {
      case (dim, (_n1, _afs)) => _n1.samples(dim) match { case (_n2, _af) => (_n2, _af :: _afs) }
    }
    val a = afs.map(af => Nd4j.create(af.toArray))
    val (uniform, b) = UniformDist(w / 2, w / 2, normal.rng).sample
    (apply(a, b, w), uniform.rng)
  }

}
