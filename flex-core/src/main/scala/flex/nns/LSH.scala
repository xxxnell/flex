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

trait LSHOps[V] extends LSHLaws[V] {

  /**
   * a * x
   * */
  def mul(lsh: LSH[V], x: V): Vec

  def shape(lsh: LSH[V]): (Int, Int)

}

trait LSHLaws[V] { self: LSHOps[V] =>

  def hashf(lsh: LSH[V], x: V): Vec = mul(lsh, x).add(lsh.b).div(lsh.w)

  def hashList(lsh: LSH[V], x: V): List[Int] = {
    val hashf = self.hashf(lsh, x)
    (0 until size(lsh)).toList.map(i => hashf.getFloat(i.toLong).floor.round)
  }

  def hashVec(lsh: LSH[V], x: V): Vec = self.hashf(lsh, x).floor.round

  def dim(lsh: LSH[V]): Int = shape(lsh)._2

  def size(lsh: LSH[V]): Int = shape(lsh)._1

}

trait LSHSyntax {

  implicit class LSHSyntaxImpl[V](lsh: LSH[V]) {
    def hash(x: V)(implicit ops: LSHOps[V]): List[Int] = ops.hashList(lsh, x)
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
