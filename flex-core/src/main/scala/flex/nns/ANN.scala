package flex.nns

import flex.util.IdentityHashMap
import flex.util.IdentityHashMap.syntax._
import flex.util.IdentityHashSet
import flex.util.IdentityHashSet.syntax._

import flex.vec._

trait ANN[V] {

  type HTable = IdentityHashMap[Int, IdentityHashSet[V]]

  type VTable = IdentityHashMap[V, Int]

  val lsh: LSH[V]

  val htables: List[ANN[V]#HTable]

  val vtables: List[ANN[V]#VTable]

}

trait ANNOps[V] extends ANNLaws[V] {

  def patchHTables(ann: ANN[V], table: List[ANN[V]#HTable]): ANN[V]

  def patchVTables(ann: ANN[V], table: List[ANN[V]#VTable]): ANN[V]

  def distance(x1: V, x2: V): Float

}

trait ANNLaws[V] { self: ANNOps[V] =>

  def add(ann: ANN[V], xs: List[V]): ANN[V] = xs.foldLeft(ann) {
    case (_ann, x) =>
      val hashs = _ann.lsh.hash(x)
      val hts1 = hashs.zip(_ann.htables).map {
        case (h, ht) => ht.updated(h, ht.getOrElse(h, IdentityHashSet.empty[V]).+(x))
      }
      val vts1 = hashs.zip(_ann.vtables).map { case (h, vt) => vt.updated(x, h) }
      patchVTables(patchHTables(_ann, hts1), vts1)
  }

  def remove(ann: ANN[V], x: V): ANN[V] = {
    val hashs = ann.vtables.map(vtable => vtable.get(x))
    val vtables1 = ann.vtables.map(vtable => vtable.-(x))
    val htables1 = hashs.zip(ann.htables).map {
      case (hasho, htable) => hasho.fold(htable)(hash => htable.-(hash))
    }
    patchVTables(patchHTables(ann, htables1), vtables1)
  }

  def search(ann: ANN[V], x: V): Option[V] = {
    val hashs = ann.lsh.hash(x)
    val vecs = hashs.zip(ann.htables).flatMap { case (h, ht) => ht.getOrElse(h, IdentityHashSet.empty[V]).toList }
    val neighbors = frequentest(vecs)
    if (neighbors.size > 2) neighbors.map(n => (n, distance(x, n))).sortBy(_._2).headOption.map(_._1)
    else neighbors.headOption
  }

  def frequentest[A](as: List[A]): List[A] = {
    val ranks = as
      .foldRight(IdentityHashMap.empty[A, Int]) { case (_a, _ranks) => _ranks.updated(_a, _ranks.getOrElse(_a, 0) + 1) }
      .inner
    lazy val (_, maxRank) = ranks.maxBy(_._2)
    ranks.filter { case (_, rank) => rank >= maxRank }.keys.toList.map(_.a)
  }

  def isEmpty(ann: ANN[_]): Boolean =
    ann.htables.exists(htable => htable.isEmpty) || ann.vtables.exists(vtable => vtable.isEmpty)

  def size(ann: ANN[V]): Int = ann.lsh.shape._1

  def dim(ann: ANN[V]): Int = ann.lsh.shape._2

}

trait ANNSyntax {

  implicit class AnnSyntaxImpl[V](ann: ANN[V]) {
    def add(x: V)(implicit ops: ANNOps[V]): ANN[V] = ops.add(ann, x :: Nil)
    def adds(xs: List[V])(implicit ops: ANNOps[V]): ANN[V] = ops.add(ann, xs)
    def remove(x: V)(implicit ops: ANNOps[V]): ANN[V] = ops.remove(ann, x)
    def search(x: V)(implicit ops: ANNOps[V]): Option[V] = ops.search(ann, x)
    def isEmpty(implicit ops: ANNOps[V]): Boolean = ops.isEmpty(ann)
    def size(implicit ops: ANNOps[V]): Int = ops.size(ann)
    def dim(implicit ops: ANNOps[V]): Int = ops.dim(ann)
  }

  implicit val vecOps: ANNOps[Vec] = VecANN
  implicit val sumVecOps: ANNOps[SumVec] = SumVecANN

}

object ANN {

  object syntax extends ANNSyntax with ParANNSyntax

}
