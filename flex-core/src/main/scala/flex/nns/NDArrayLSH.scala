package flex.nns

import flex.pdf.{NormalDist, UniformDist}
import flex.rand.IRng
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

trait NDArrayLSHOps extends LSHOps[INDArray] {

  def hash(lsh: LSH[INDArray], x: INDArray): Int = ((lsh.a.mul(x).getFloat(0) + lsh.b) / lsh.w).floor.round

  def dim(lsh: LSH[INDArray]): Int = lsh.a.shape.apply(1).toInt

}

object NDArrayLSH extends NDArrayLSHOps {

  private case class NDArrayLSHImpl(a: INDArray, b: Float, w: Float) extends LSH[INDArray]

  def apply(a: INDArray, b: Float, w: Float): NDArrayLSH = NDArrayLSHImpl(a, b, w)

  def apply(dim: Int, w: Float, rng: IRng): (NDArrayLSH, IRng) = {
    val (normal, af) = NormalDist(0.0, 1.0, rng).samples(dim)
    val a = Nd4j.create(af.toArray)
    val (uniform, b) = UniformDist(w / 2, w / 2, normal.rng).sample
    (apply(a, b, w), uniform.rng)
  }

}
