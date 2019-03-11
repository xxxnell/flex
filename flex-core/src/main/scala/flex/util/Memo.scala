package flex.util

import scala.collection.mutable

/**
 * NOTE: `Memo` is mutable object.
 * */
trait Memo[K, V] {

  val size: Int

  var full = false

  val queue: mutable.Queue[K]

  val table: mutable.WeakHashMap[K, V]

}

trait MemoOps {

  def get[K, V](memo: Memo[K, V], k: K, f: K => V): V =
    memo.table.get(k) match {
      case Some(value) => value
      case None =>
        val value = f(k)
        put(memo, k, value)
        value
    }

  def put[K, V](memo: Memo[K, V], k: K, v: V): Unit = {
    memo.queue += k
    memo.table += ((k, v))
    if (!memo.full && memo.queue.size >= memo.size) memo.full = true
    if (memo.full && memo.queue.nonEmpty) memo.table.remove(memo.queue.dequeue())
  }

}

trait MemoSyntax {

  implicit class MemoSyntaxImpl[K, V](memo: Memo[K, V]) {
    def get(k: K, f: K => V): V = Memo.get(memo, k, f)
  }

}

object Memo extends MemoOps {

  private case class MemoImpl[K, V](size: Int, queue: mutable.Queue[K], table: mutable.WeakHashMap[K, V])
      extends Memo[K, V]

  case object syntax extends MemoSyntax

  def bare[K, V](size: Int, queue: mutable.Queue[K], table: mutable.WeakHashMap[K, V]): Memo[K, V] =
    MemoImpl(size, queue, table)

  def empty[K, V](size: Int): Memo[K, V] = bare(size, mutable.Queue.empty[K], mutable.WeakHashMap.empty[K, V])

}
