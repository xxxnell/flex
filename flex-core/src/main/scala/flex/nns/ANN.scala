package flex.nns

import flex.util.IdentityHashMap
import flex.util.IdentityHashMap.syntax._
import flex.util.IdentityHashSet
import flex.util.IdentityHashSet.syntax._

import scala.language.higherKinds

trait ANN[V] {

  val lsh: LSH[V]

  val htables: List[HTable[V]]

  val vtables: List[VTable[V]]

}

trait ANNOps[V, A <: ANN[V]] extends ANNLaws[V, A] {

  def patchHTables(ann: A, table: List[HTable[V]]): A

  def patchVTables(ann: A, table: List[VTable[V]]): A

  def distance(x1: V, x2: V): Float

}

trait ANNLaws[V, A <: ANN[V]] { self: ANNOps[V, A] =>

  def add(ann: A, xs: List[V]): A = xs.foldLeft(ann) {
    case (_ann, x) =>
      val hashs = _ann.lsh.hash(x)
      val hts1 = hashs.zip(_ann.htables).map {
        case (h, ht) => ht.updated(h, ht.getOrElse(h, IdentityHashSet.empty[V]).+(x))
      }
      val vts1 = hashs.zip(_ann.vtables).map { case (h, vt) => vt.updated(x, h) }
      patchVTables(patchHTables(_ann, hts1), vts1)
  }

  def remove(ann: A, x: V): A = {
    val hashs = ann.vtables.map(vtable => vtable.get(x))
    val vtables1 = ann.vtables.map(vtable => vtable.-(x))
    val htables1 = hashs.zip(ann.htables).map {
      case (hasho, htable) => hasho.fold(htable)(hash => htable.-(hash))
    }
    patchVTables(patchHTables(ann, htables1), vtables1)
  }

  def search(ann: A, x: V): Option[V] = {
    val hashs = ann.lsh.hash(x)
    val vecs = hashs.zip(ann.htables).flatMap { case (h, ht) => ht.getOrElse(h, IdentityHashSet.empty[V]).toList }
    val neighbors = frequentest(vecs)
    if (neighbors.size > 2) neighbors.map(n => (n, distance(x, n))).sortBy(_._2).headOption.map(_._1)
    else neighbors.headOption
  }

  def frequentest[X](xs: List[X]): List[X] = {
    val ranks = xs
      .foldRight(IdentityHashMap.empty[X, Int]) { case (x, _ranks) => _ranks.updated(x, _ranks.getOrElse(x, 0) + 1) }
      .inner
    lazy val (_, maxRank) = ranks.maxBy(_._2)
    ranks.filter { case (_, rank) => rank >= maxRank }.keys.toList.map(_.a)
  }

  def isEmpty(ann: ANN[_]): Boolean =
    ann.htables.exists(htable => htable.isEmpty) || ann.vtables.exists(vtable => vtable.isEmpty)

  def size(ann: ANN[_]): Int = ann.lsh.shape._1

  def dim(ann: ANN[_]): Int = ann.lsh.shape._2

  def vs(ann: A): List[V] = ann.vtables.headOption.map(vtable => vtable.inner.keySet.toList.map(_.a)).getOrElse(Nil)

  def clear(ann: A): Unit = ann.lsh.clear

}

trait ANNSyntax extends VecANNSyntax with SumVecANNSyntax

trait ANNSyntaxImpl[V, A <: ANN[V]] {
  val ops: ANNOps[V, A]
  val ann: A
  def add(x: V): A = ops.add(ann, x :: Nil)
  def adds(xs: List[V]): A = ops.add(ann, xs)
  def remove(x: V): A = ops.remove(ann, x)
  def search(x: V): Option[V] = ops.search(ann, x)
  def isEmpty: Boolean = ops.isEmpty(ann)
  def size: Int = ops.size(ann)
  def dim: Int = ops.dim(ann)
  def vs: List[V] = ops.vs(ann)
  def clear: Unit = ops.clear(ann)
}

object ANN {

  object syntax extends ANNSyntax with ParANNSyntax

}
