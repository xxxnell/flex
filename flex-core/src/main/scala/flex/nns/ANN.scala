package flex.nns

import flex.nns.LSH.syntax._
import flex.util.IdentityHashMap
import flex.util.IdentityHashMap.syntax._
import flex.util.IdentityHashSet
import flex.util.IdentityHashSet.syntax._

import flex.vec._

trait ANN[V] {

  type HTable = IdentityHashMap[Int, IdentityHashSet[V]]

  type VTable = IdentityHashMap[V, Int]

  val lshs: List[LSH[V]]

  val htables: List[ANN[V]#HTable]

  val vtables: List[ANN[V]#VTable]

}

trait ANNOps[V] extends ANNLaws[V] {

  def patchHTables(ann: ANN[V], table: List[ANN[V]#HTable]): ANN[V]

  def patchVTables(ann: ANN[V], table: List[ANN[V]#VTable]): ANN[V]

  def distance(x1: V, x2: V): Float

}

trait ANNLaws[V] { self: ANNOps[V] =>

  def add(ann: ANN[V], xs: List[V])(implicit lshOps: LSHOps[V]): ANN[V] = xs.foldLeft(ann) {
    case (_ann, x) =>
      val hcs = _ann.lshs.map(lsh => lsh.hash(x))
      val hts1 = hcs.zip(_ann.htables).map {
        case (h, ht) =>
          ht.updated(h, ht.getOrElse(h, IdentityHashSet.empty[V]).+(x))
      }
      val vts1 = hcs.zip(_ann.vtables).map { case (h, vt) => vt.updated(x, h) }
      patchVTables(patchHTables(_ann, hts1), vts1)
  }

  def remove(ann: ANN[V], x: V): ANN[V] = {
    val hcs = ann.vtables.map(vt => vt.get(x))
    val hts1 = hcs.zip(ann.htables).map {
      case (ho, ht) => ho.flatMap(h => ht.get(h).map(vs => ht.updated(h, vs.-(x)))).getOrElse(ht)
    }
    val vts1 = ann.vtables.map(vt => vt.-(x))
    patchVTables(patchHTables(ann, hts1), vts1)
  }

  def search(ann: ANN[V], x: V)(implicit lshOps: LSHOps[V]): Option[V] = {
    val hashs = ann.lshs.map(lsh => lsh.hash(x))
    val vecs = hashs.zip(ann.htables).flatMap { case (h, ht) => ht.getOrElse(h, IdentityHashSet.empty[V]).toList }
    val neighbors = frequentest(vecs)
    neighbors.map(n => (n, distance(x, n))).sortBy(_._2).headOption.map(_._1)
  }

  def frequentest[A](as: List[A]): List[A] = {
    val ranks = as
      .foldRight(IdentityHashMap.empty[A, Int]) { case (a, _ranks) => _ranks.updated(a, _ranks.getOrElse(a, 0) + 1) }
      .toMap
    val (_, maxRank) = ranks.maxBy(_._2)
    ranks.filter { case (_, rank) => rank >= maxRank }.keys.toList
  }

  def isEmpty(ann: ANN[_]): Boolean =
    ann.htables.exists(htable => htable.isEmpty) || ann.vtables.exists(vtable => vtable.isEmpty)

  def l(ann: ANN[_]): Int = ann.lshs.size

  def dim(ann: ANN[V])(implicit ops: LSHOps[V]): Int = ann.lshs.headOption.map(_.dim).getOrElse(0)

}

trait ANNSyntax {

  implicit class AnnSyntaxImpl[V](ann: ANN[V]) {
    def add(x: V)(implicit ops: ANNOps[V], lshOps: LSHOps[V]): ANN[V] = ops.add(ann, x :: Nil)
    def adds(xs: List[V])(implicit ops: ANNOps[V], lshOps: LSHOps[V]): ANN[V] = ops.add(ann, xs)
    def remove(x: V)(implicit ops: ANNOps[V]): ANN[V] = ops.remove(ann, x)
    def search(x: V)(implicit ops: ANNOps[V], lshOps: LSHOps[V]): Option[V] = ops.search(ann, x)
    def isEmpty(implicit ops: ANNOps[V]): Boolean = ops.isEmpty(ann)
    def l(implicit ops: ANNOps[V]): Int = ops.l(ann)
    def dim(implicit ops: ANNOps[V], lshOps: LSHOps[V]): Int = ops.dim(ann)
  }

  implicit val vecOps: ANNOps[Vec] = VecANN
  implicit val sumVecOps: ANNOps[SumVec] = SumVecANN

}

object ANN {

  object syntax extends ANNSyntax with ParANNSyntax

}
