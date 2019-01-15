package flex.nns

import flex.nns.LSH.syntax._
import flex.pdf.{SumVec, Vec}

import scala.collection.immutable.{HashMap, HashSet}

trait ANN[V] {

  type HTable = HashMap[Int, HashSet[V]]

  type VTable = HashMap[V, Int]

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

  def add(ann: ANN[V], x: V)(implicit lshOps: LSHOps[V]): ANN[V] = {
    val hashcodes = ann.lshs.map(lsh => lsh.hash(x))
    val htables1 = hashcodes.zip(ann.htables).map { case (h, ht) => ht.updated(h, ht.getOrElse(h, HashSet[V]()).+(x)) }
    val vtables1 = hashcodes.zip(ann.vtables).map { case (h, vt) => vt.updated(x, h) }
    patchVTables(patchHTables(ann, htables1), vtables1)
  }

  def remove(ann: ANN[V], x: V): ANN[V] = {
    val hashcodes = ann.vtables.map(vt => vt.get(x))
    val htables1 = hashcodes.zip(ann.htables).map {
      case (ho, ht) => ho.flatMap(h => ht.get(h).map(vs => ht.updated(h, vs.-(x)))).getOrElse(ht)
    }
    val vtables1 = ann.vtables.map(vt => vt.-(x))
    patchVTables(patchHTables(ann, htables1), vtables1)
  }

  def search(ann: ANN[V], x: V)(implicit lshOps: LSHOps[V]): Option[V] = {
    val hashcodes = ann.lshs.map(lsh => lsh.hash(x))
    val vectors = hashcodes.zip(ann.htables).flatMap { case (h, htable) => htable.getOrElse(h, Nil) }
    val counts = vectors.groupBy(identity).mapValues(_.size)
    val ranks = counts.groupBy { case (_, r) => r }.mapValues(_.keySet)
    val neighbors = ranks.toSeq.sortWith { case ((rank1, _), (rank2, _)) => rank1 > rank2 }.headOption.map(_._2)
    neighbors.flatMap(ns => ns.map(n => (n, distance(x, n))).toSeq.sortBy(_._2).headOption.map(_._1))
  }

  def isEmpty(ann: ANN[_]): Boolean =
    ann.htables.exists(htable => htable.isEmpty) || ann.vtables.exists(vtable => vtable.isEmpty)

}

trait ANNSyntax {

  implicit class AnnSyntaxImpl[V](ann: ANN[V]) {
    def add(x: V)(implicit ops: ANNOps[V], lshOps: LSHOps[V]): ANN[V] = ops.add(ann, x)
    def remove(x: V)(implicit ops: ANNOps[V]): ANN[V] = ops.remove(ann, x)
    def search(x: V)(implicit ops: ANNOps[V], lshOps: LSHOps[V]): Option[V] = ops.search(ann, x)
    def isEmpty(implicit ops: ANNOps[V]): Boolean = ops.isEmpty(ann)
  }

  implicit val vecOps: ANNOps[Vec] = VecANN
  implicit val sumVecOps: ANNOps[SumVec] = SumVecANN

}

object ANN {

  object syntax extends ANNSyntax with ParANNSyntax

}
