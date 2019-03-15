package flex.util

import cats.implicits._
import flex.rand._
import flex.util.IdentityHashMap.syntax._

import scala.collection.immutable.HashMap

trait RandomIdentitySet[A] {

  val as: Vector[A]

  val is: IdentityHashMap[A, Int]

  val rng: IRng

}

trait RandomSetOps {

  def patchRng[A](as: RandomIdentitySet[A], rng: IRng): RandomIdentitySet[A] = RandomIdentitySet(as.as, as.is, rng)

  def contains[A](as: RandomIdentitySet[A], a: A): Boolean = as.is.contains(a)

  def size(as: RandomIdentitySet[_]): Int = as.as.size

  def isEmpty(as: RandomIdentitySet[_]): Boolean = size(as) == 0

  def add[A](as: RandomIdentitySet[A], a: A): RandomIdentitySet[A] =
    if (!contains(as, a)) {
      val as1 = as.as.:+(a)
      val is1 = as.is.+(a -> (as1.size - 1))

      RandomIdentitySet(as1, is1, as.rng)
    } else as

  def remove[A](as: RandomIdentitySet[A], a: A): RandomIdentitySet[A] = as.is.get(a).fold(as) { i =>
    val last = as.as.last
    val as1 = as.as.updated(i, last).dropRight(1)
    val is1 = as.is.+(last -> i).-(a)

    RandomIdentitySet(as1, is1, as.rng)
  }

  def rand[A](as: RandomIdentitySet[A]): (RandomIdentitySet[A], Option[A]) =
    if (!isEmpty(as)) as.rng.nextInt(size(as)).bimap(rng => patchRng(as, rng), rnd => as.as.apply(rnd).some)
    else (as, None)

  def toList[A](as: RandomIdentitySet[A]): List[A] = as.as.toList

}

trait RandomSetSyntax {

  implicit class RandomSetSyntaxImpl[A](as: RandomIdentitySet[A]) {
    def patchRng(rng: IRng): RandomIdentitySet[A] = RandomIdentitySet.patchRng(as, rng)
    def contains(a: A): Boolean = RandomIdentitySet.contains(as, a)
    def size: Int = RandomIdentitySet.size(as)
    def isEmpty: Boolean = RandomIdentitySet.isEmpty(as)
    def add(a: A): RandomIdentitySet[A] = RandomIdentitySet.add(as, a)
    def +(a: A): RandomIdentitySet[A] = RandomIdentitySet.add(as, a)
    def remove(a: A): RandomIdentitySet[A] = RandomIdentitySet.remove(as, a)
    def -(a: A): RandomIdentitySet[A] = RandomIdentitySet.remove(as, a)
    def rand: (RandomIdentitySet[A], Option[A]) = RandomIdentitySet.rand(as)
    def toList: List[A] = RandomIdentitySet.toList(as)
  }

}

object RandomIdentitySet extends RandomSetOps {

  object syntax extends RandomSetSyntax

  private case class RandomIdentitySetImpl[A](as: Vector[A], is: IdentityHashMap[A, Int], rng: IRng)
      extends RandomIdentitySet[A]

  def apply[A](as: Vector[A], is: IdentityHashMap[A, Int], rng: IRng): RandomIdentitySet[A] =
    RandomIdentitySetImpl(as, is, rng)

  def apply[A](as: List[A], rng: IRng): RandomIdentitySet[A] = as.foldLeft(empty[A](rng)) { case (s, a) => add(s, a) }

  def empty[A](rng: IRng): RandomIdentitySet[A] =
    RandomIdentitySetImpl(Vector.empty[A], IdentityHashMap.empty[A, Int], rng)

}
