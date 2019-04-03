package flex.nns

import flex.util.Memo.syntax._

/**
 * Stable distribution LSH.
 * h(x) = ⌊ (a * x + b) / w ⌋, where a ~ N(0, 1) and b ~ U[0,w] for given w.
 *
 * @see https://en.wikipedia.org/wiki/Locality-sensitive_hashing#Stable_distributions
 * @see http://mlwiki.org/index.php/Euclidean_LSH
 * */
trait LSH[V] extends LSHOps[V] {

  val a: List[V]

  val b: List[Float]

  val w: List[Float]

  val memo: LSHMemo

}

trait LSHOps[V] { lsh: LSH[V] =>

  /**
   * a * x
   * */
  def mul(x: V): List[Float]

  def shape: (Int, Int)

  // laws

  def hash(x: V): List[Int] = (mul(x), b, w).zipped.map {
    case (_ax, _b, _w) => ((_ax + _b) / _w).floor.round
  }

  def dim: Int = shape._2

  def size: Int = shape._1

  def clear: Unit = memo.clear

}

object LSH
