package flex.nns

import flex.pdf._
import flex.rand._
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

/**
 * Stable distribution LSH.
 * h(x) = ⌊ (a * x + b) / w ⌋, where a ~ N(0, 1) and b ~ U[0,w] for given w.
 *
 * @see https://en.wikipedia.org/wiki/Locality-sensitive_hashing#Stable_distributions
 * @see http://mlwiki.org/index.php/Euclidean_LSH
 * */
trait LSH[V] {

  def hash(x: V): Int

}

trait NDArrayLSH extends LSH[INDArray] {

  val a: INDArray

  val b: Float

  val w: Float

  def hash(x: INDArray): Int = ((a.mul(x).getFloat(0) + b) / w).floor.round

}

trait CodewordLSH extends LSH[VQH#Codeword] {

  val a: VQH#Codeword

  val b: Float

  val w: Float

  def hash(x: VQH#Codeword): Int = ((x.zip(a).map { case (_x, _a) => _a.mul(_x).getFloat(0) }.sum + b) / w).floor.round

}

object LSH {

  private case class NDArrayLSHImpl(a: INDArray, b: Float, w: Float) extends NDArrayLSH

  private case class CodewordLSHImpl(a: VQH#Codeword, b: Float, w: Float) extends CodewordLSH

  def ndarray(a: INDArray, b: Float, w: Float): NDArrayLSH = NDArrayLSHImpl(a, b, w)

  def codeword(a: VQH#Codeword, b: Float, w: Float): CodewordLSH = CodewordLSHImpl(a, b, w)

  def ndarray(dim: Int, w: Float, rng: IRng): (NDArrayLSH, IRng) = {
    val (normal, af) = NormalDist(0.0, 1.0, rng).samples(dim)
    val a = Nd4j.create(af.toArray)
    val (uniform, b) = UniformDist(w / 2, w / 2, normal.rng).sample
    (ndarray(a, b, w), uniform.rng)
  }

  def codeword(dims: List[Int], w: Float, rng: IRng): (CodewordLSH, IRng) = {
    val (normal, afs) = dims.foldRight[(Dist[Double], List[List[Double]])]((NormalDist(0.0, 1.0, rng), Nil)) {
      case (dim, (_n1, _afs)) => _n1.samples(dim) match { case (_n2, _af) => (_n2, _af :: _afs) }
    }
    val a = afs.map(af => Nd4j.create(af.toArray))
    val (uniform, b) = UniformDist(w / 2, w / 2, normal.rng).sample
    (codeword(a, b, w), uniform.rng)
  }

}
