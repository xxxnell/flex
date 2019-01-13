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

  val a: V

  val b: Float

  val w: Float

}

trait LSHOps[V] {

  def hash(lsh: LSH[V], x: V): Int

}

trait LSHSyntax {

  implicit class LSHSyntaxImpl[V](lsh: LSH[V]) {
    def hash(x: V)(implicit ops: LSHOps[V]): Int = ops.hash(lsh, x)
  }

  implicit val ndarrayLsh: LSHOps[INDArray] = NDArrayLSH
  implicit val codewordLsh: LSHOps[VQH#Codeword] = CodewordLSH

}

object LSH {

  object syntax extends LSHSyntax

}
