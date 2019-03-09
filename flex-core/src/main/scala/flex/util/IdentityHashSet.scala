package flex.util

import scala.collection.immutable.HashSet

trait IdentityHashSet[A] {

  val inner: HashSet[EqAdapter[A]]

  override def toString: String = IdentityHashSet.toList(this).toString()

}

trait IdentityHashSetOps {

  def add[A](set: IdentityHashSet[A], a: A): IdentityHashSet[A] = IdentityHashSet(set.inner.+(EqAdapter(a)))

  def remove[A](set: IdentityHashSet[A], a: A): IdentityHashSet[A] = IdentityHashSet(set.inner.-(EqAdapter(a)))

  def exists[A](set: IdentityHashSet[A], a: A): Boolean = set.inner.contains(EqAdapter(a))

  def size[A](set: IdentityHashSet[A]): Int = set.inner.size

  def isEmpty[A](set: IdentityHashSet[A]): Boolean = set.inner.isEmpty

  def toList[A](set: IdentityHashSet[A]): List[A] = set.inner.map(_.a).toList

}

trait IdentityHashSetSyntax {

  implicit class IdentityHashSetSyntaxImpl[A](set: IdentityHashSet[A]) {
    def add(a: A): IdentityHashSet[A] = IdentityHashSet.add(set, a)
    def +(a: (A)): IdentityHashSet[A] = IdentityHashSet.add(set, a)
    def remove(a: A): IdentityHashSet[A] = IdentityHashSet.remove(set, a)
    def -(a: A): IdentityHashSet[A] = IdentityHashSet.remove(set, a)
    def exists(a: A): Boolean = IdentityHashSet.exists(set, a)
    def size: Int = IdentityHashSet.size(set)
    def isEmpty: Boolean = IdentityHashSet.isEmpty(set)
    def toList: List[A] = IdentityHashSet.toList(set)
  }

}

object IdentityHashSet extends IdentityHashSetOps {

  object syntax extends IdentityHashSetSyntax

  private case class IdentityHashSetImpl[A](inner: HashSet[EqAdapter[A]]) extends IdentityHashSet[A]

  def apply[A](inner: HashSet[EqAdapter[A]]): IdentityHashSet[A] = IdentityHashSetImpl(inner)

  def empty[A]: IdentityHashSet[A] = apply(HashSet.empty[EqAdapter[A]])

}
