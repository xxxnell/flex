package flex.nns

import scala.collection.immutable.{HashMap, HashSet}

trait ANN[V] {

  type HTable = HashMap[Int, HashSet[V]]

  type VTable = HashMap[V, Int]

  val lshs: List[LSH[V]]

  val htables: List[ANN[V]#HTable]

  val vtables: List[ANN[V]#VTable]

}

trait ANNOps[V, A <: ANN[V]] extends ANNLaws[V, A] {

  def patchHTables(ann: A, table: List[ANN[V]#HTable]): A

  def patchVTables(ann: A, table: List[ANN[V]#VTable]): A

  def distance(x1: V, x2: V): Float

}

trait ANNLaws[V, A <: ANN[V]] { self: ANNOps[V, A] =>

  def add(ann: A, x: V): A = {
    val hashcodes = ann.lshs.map(lsh => lsh.hash(x))
    val htables1 = hashcodes.zip(ann.htables).map { case (h, ht) => ht.updated(h, ht.getOrElse(h, HashSet[V]()).+(x)) }
    val vtables1 = hashcodes.zip(ann.vtables).map { case (h, vt) => vt.updated(x, h) }
    patchVTables(patchHTables(ann, htables1), vtables1)
  }

  def remove(ann: A, x: V): A = {
    val hashcodes = ann.vtables.map(vt => vt.get(x))
    val htables1 = hashcodes.zip(ann.htables).map {
      case (ho, ht) => ho.flatMap(h => ht.get(h).map(vs => ht.updated(h, vs.-(x)))).getOrElse(ht)
    }
    val vtables1 = ann.vtables.map(vt => vt.-(x))
    patchVTables(patchHTables(ann, htables1), vtables1)
  }

  def search(ann: A, x: V): Option[V] = {
    val hashcodes = ann.vtables.map(vt => vt.get(x))
    val vectors = hashcodes.zip(ann.htables).flatMap { case (ho, ht) => ho.flatMap(h => ht.get(h)).getOrElse(Nil) }
    val ranks = vectors.groupBy(identity).mapValues(_.size).groupBy { case (_, r) => r }.mapValues(_.keySet)
    val neighbors = ranks.toSeq.sortWith { case ((rank1, _), (rank2, _)) => rank1 > rank2 }.headOption.map(_._2)
    neighbors.flatMap(ns => ns.map(n => (n, distance(x, n))).toSeq.sortBy(_._2).headOption.map(_._1))
  }

}
