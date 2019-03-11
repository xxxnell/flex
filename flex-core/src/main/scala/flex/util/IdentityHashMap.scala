package flex.util

import scala.collection.immutable.HashMap
import cats.implicits._

trait IdentityHashMap[K, V] {

  val inner: HashMap[EqAdapter[K], V]

  override def toString: String = this.inner.toString

}

trait IdentityHashMapOps {

  def add[K, V](map: IdentityHashMap[K, V], kv: (K, V)): IdentityHashMap[K, V] =
    IdentityHashMap(map.inner.+(kv.leftMap(k => EqAdapter(k))))

  def remove[K, V](map: IdentityHashMap[K, V], k: K): IdentityHashMap[K, V] =
    IdentityHashMap(map.inner.-(EqAdapter(k)))

  def get[K, V](map: IdentityHashMap[K, V], k: K): Option[V] = map.inner.get(EqAdapter(k))

  def getOrElse[K, V](map: IdentityHashMap[K, V], k: K, default: => V): V = get(map, k).getOrElse(default)

  def size[K, V](map: IdentityHashMap[K, V]): Int = map.inner.size

  def isEmpty[K, V](map: IdentityHashMap[K, V]): Boolean = map.inner.isEmpty

  def toMap[K, V](map: IdentityHashMap[K, V]): Map[K, V] = map.inner.map { case (ek, v) => (ek.a, v) }

  def contains[K, V](map: IdentityHashMap[K, V], k: K): Boolean = map.inner.contains(EqAdapter(k))

  def map[K1, V1, K2, V2](map: IdentityHashMap[K1, V1], f: (K1, V1) => (K2, V2)): IdentityHashMap[K2, V2] =
    IdentityHashMap(map.inner.map { case (ek, v) => f(ek.a, v).leftMap(k => EqAdapter(k)) })

}

trait IdentityHashMapSyntax {

  implicit class IdentityHashMapSyntaxImpl[K, V](map: IdentityHashMap[K, V]) {
    def add(kv: (K, V)): IdentityHashMap[K, V] = IdentityHashMap.add(map, kv)
    def +(kv: (K, V)): IdentityHashMap[K, V] = IdentityHashMap.add(map, kv)
    def remove(k: K): IdentityHashMap[K, V] = IdentityHashMap.remove(map, k)
    def -(k: K): IdentityHashMap[K, V] = IdentityHashMap.remove(map, k)
    def get(k: K): Option[V] = IdentityHashMap.get(map, k)
    def getOrElse(k: K, default: => V): V = IdentityHashMap.getOrElse(map, k, default)
    def updated(k: K, v: V): IdentityHashMap[K, V] = IdentityHashMap.add(map, (k, v))
    def size: Int = IdentityHashMap.size(map)
    def isEmpty: Boolean = IdentityHashMap.isEmpty(map)
    def toMap: Map[K, V] = IdentityHashMap.toMap(map)
    def contains(k: K): Boolean = IdentityHashMap.contains(map, k)
    def map[K2, V2](f: (K, V) => (K2, V2)): IdentityHashMap[K2, V2] = IdentityHashMap.map(map, f)
  }

}

object IdentityHashMap extends IdentityHashMapOps {

  object syntax extends IdentityHashMapSyntax

  private case class IdentityHashMapImpl[K, V](inner: HashMap[EqAdapter[K], V]) extends IdentityHashMap[K, V]

  def apply[K, V](inner: HashMap[EqAdapter[K], V]): IdentityHashMap[K, V] = IdentityHashMapImpl(inner)

  def apply[K, V](kv: (K, V)*): IdentityHashMap[K, V] = apply(HashMap(kv.map(_.leftMap(k => EqAdapter(k))): _*))

  def empty[K, V]: IdentityHashMap[K, V] = apply(HashMap.empty[EqAdapter[K], V])

}
