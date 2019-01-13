package flex.nns

import flex.pdf.{Dist, NormalDist, UniformDist, VQH}
import flex.rand.IRng
import org.nd4j.linalg.factory.Nd4j

trait CodewordLSHOps extends LSHOps[VQH#Codeword] {

  def hash(lsh: CodewordLSH, x: VQH#Codeword): Int =
    ((x.zip(lsh.a).map { case (_x, _a) => _a.mul(_x).getFloat(0) }.sum + lsh.b) / lsh.w).floor.round

  def dim(lsh: CodewordLSH): Int = lsh.a.map(_.shape.apply(1)).sum.toInt

}

object CodewordLSH extends CodewordLSHOps {

  private case class CodewordLSHImpl(a: VQH#Codeword, b: Float, w: Float) extends CodewordLSH

  def apply(a: VQH#Codeword, b: Float, w: Float): CodewordLSH = CodewordLSHImpl(a, b, w)

  def apply(dims: List[Int], w: Float, rng: IRng): (CodewordLSH, IRng) = {
    val (normal, afs) = dims.foldRight[(Dist[Double], List[List[Double]])]((NormalDist(0.0, 1.0, rng), Nil)) {
      case (dim, (_n1, _afs)) => _n1.samples(dim) match { case (_n2, _af) => (_n2, _af :: _afs) }
    }
    val a = afs.map(af => Nd4j.create(af.toArray))
    val (uniform, b) = UniformDist(w / 2, w / 2, normal.rng).sample
    (apply(a, b, w), uniform.rng)
  }

}
