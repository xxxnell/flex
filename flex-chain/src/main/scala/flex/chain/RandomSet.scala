package flex.chain

import scala.collection.immutable.HashMap
import flex.rand._
import cats._
import cats.implicits._

trait RandomSet[A] {

  val as: Vector[A]

  val is: HashMap[A, Int]

  val rng: IRng

}

trait RandomSetOps {

  def patchRng[A](as: RandomSet[A], rng: IRng): RandomSet[A] = RandomSet(as.as, as.is, rng)

  def contains[A](as: RandomSet[A], a: A): Boolean = as.is.contains(a)

  def size(as: RandomSet[_]): Int = as.as.size

  def isEmpty(as: RandomSet[_]): Boolean = size(as) == 0

  def add[A](as: RandomSet[A], a: A): RandomSet[A] =
    if (!contains(as, a)) {
      val as1 = as.as.+:(a)
      val is1 = as.is.+(a -> (as1.size - 1))

      RandomSet(as1, is1, as.rng)
    } else as

  def remove[A](as: RandomSet[A], a: A): RandomSet[A] = as.is.get(a).fold(as) { i =>
    val last = as.as.last
    val as1 = as.as.updated(i, last).dropRight(1)
    val is1 = as.is.+(last -> i).-(a)

    RandomSet(as1, is1, as.rng)
  }

  def rand[A](as: RandomSet[A]): (RandomSet[A], Option[A]) =
    if (!isEmpty(as)) as.rng.nextInt(size(as)).bimap(rng => patchRng(as, rng), rnd => as.as.apply(rnd).some)
    else (as, None)

}

trait RandomSetSyntax {

  implicit class RandomSetSyntaxImpl[A](as: RandomSet[A]) {
    def contains(a: A): Boolean = RandomSet.contains(as, a)
    def size: Int = RandomSet.size(as)
    def add(a: A): RandomSet[A] = RandomSet.add(as, a)
    def remove(a: A): RandomSet[A] = RandomSet.remove(as, a)
    def rand: (RandomSet[A], Option[A]) = RandomSet.rand(as)
  }

}

object RandomSet extends RandomSetOps {

  object syntax extends RandomSetSyntax

  private case class RandomSetImpl[A](as: Vector[A], is: HashMap[A, Int], rng: IRng) extends RandomSet[A]

  def apply[A](as: Vector[A], is: HashMap[A, Int], rng: IRng): RandomSet[A] = RandomSetImpl(as, is, rng)

  def empty[A](rng: IRng): RandomSet[A] = RandomSetImpl(Vector.empty[A], HashMap.empty[A, Int], rng)

}
