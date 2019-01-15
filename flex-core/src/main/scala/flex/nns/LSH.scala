package flex.nns

import flex.vec._

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

  def dim(lsh: LSH[V]): Int

}

trait LSHSyntax {

  implicit class LSHSyntaxImpl[V](lsh: LSH[V]) {
    def hash(x: V)(implicit ops: LSHOps[V]): Int = ops.hash(lsh, x)
    def dim(implicit ops: LSHOps[V]): Int = ops.dim(lsh)
  }

  implicit val vecLsh: LSHOps[Vec] = VecLSH
  implicit val sumVecLsh: LSHOps[SumVec] = SumVecLSH

}

object LSH {

  object syntax extends LSHSyntax

}
