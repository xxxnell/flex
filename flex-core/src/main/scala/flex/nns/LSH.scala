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

  val b: Vec

  val w: Vec

}

trait LSHOps[V] {

  def hashs(lsh: LSH[V], x: V): List[Int]

  def shape(lsh: LSH[V]): (Int, Int)

  def dim(lsh: LSH[V]): Int = shape(lsh)._2

  def size(lsh: LSH[V]): Int = shape(lsh)._1

}

trait LSHSyntax {

  implicit class LSHSyntaxImpl[V](lsh: LSH[V]) {
    def hashs(x: V)(implicit ops: LSHOps[V]): List[Int] = ops.hashs(lsh, x)
    def shape(implicit ops: LSHOps[V]): (Int, Int) = ops.shape(lsh)
    def dim(implicit ops: LSHOps[V]): Int = ops.dim(lsh)
    def size(implicit ops: LSHOps[V]): Int = ops.size(lsh)
  }

  implicit val vecLsh: LSHOps[Vec] = VecLSH
  implicit val sumVecLsh: LSHOps[SumVec] = SumVecLSH

}

object LSH {

  object syntax extends LSHSyntax

}
