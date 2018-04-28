package flip.pdf

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

case class Buffer[A](dataset: Queue[(A, Count)], size: Int) {

  override def toString: String = s"Buffer(size = $size, dataset = $dataset)"

  override def equals(other: Any): Boolean = other.isInstanceOf[Buffer[A]] && {
    val buffer = other.asInstanceOf[Buffer[A]]
    dataset == buffer.dataset && size == buffer.size
  }
}

trait BufferOps {

  def append[A](buffer: Buffer[A], data: (A, Count)): Buffer[A] = Buffer(buffer.dataset :+ data, buffer.size + 1)

  def appendN[A](buffer: Buffer[A], dataset: List[(A, Count)]): Buffer[A] = {
    val size = dataset.size
    var (i, _b, _d) = (0, buffer, dataset)
    while (i < size) {
      _b = append(_b, _d.head)
      _d = _d.tail
      i += 1
    }
    _b
  }

  def splitAt[A](buffer: Buffer[A], n: Int): (Buffer[A], Buffer[A]) = {
    val size = buffer.size
    val _n = if (n > size) size else if (n < 0) 0 else n
    val (_q1, _q2) = queueSplitAt(buffer.dataset, _n)

    (Buffer.apply(_q1, _n), Buffer(_q2, size - _n))
  }

  def queueSplitAt[A](queue: Queue[A], n: Int): (Queue[A], Queue[A]) = {
    var _q1: Queue[A] = Queue.empty[A]
    var _q2: Queue[A] = queue

    var i = 0
    while (i < n) {
      _q1 = _q1.enqueue(_q2.head)
      _q2 = _q2.tail
      i += 1
    }

    (_q1, _q2)
  }

  def toList[A](buffer: Buffer[A]): List[(A, Count)] = {
    if (buffer.size == 1) buffer.dataset.head :: Nil else buffer.dataset.toList
  }

  def sum[A](buffer: Buffer[A]): Count = buffer.dataset.foldLeft(0.0) { case (acc, (_, count)) => acc + count }

  def headOption[A](buffer: Buffer[A]): Option[(A, Count)] = buffer.dataset.headOption

}

trait BufferSyntax {

  implicit class BufferSyntaxImpl[A](buffer: Buffer[A]) {
    def append(data: (A, Count)): Buffer[A] = Buffer.append(buffer, data)
    def :+(data: (A, Count)): Buffer[A] = Buffer.append(buffer, data)
    def +(data: (A, Count)): Buffer[A] = Buffer.append(buffer, data)
    def ++(dataset: List[(A, Count)]): Buffer[A] = Buffer.appendN(buffer, dataset)
    def splitAt(n: Int): (Buffer[A], Buffer[A]) = Buffer.splitAt(buffer, n)
    def toList: List[(A, Count)] = Buffer.toList(buffer)
    def sum: Count = Buffer.sum(buffer)
    def headOption: Option[(A, Count)] = Buffer.headOption(buffer)
    def size: Int = buffer.size
    def isEmpty: Boolean = size == 0
    def nonEmpty: Boolean = size != 0
  }

}

object Buffer extends BufferOps {

  case object syntax extends BufferSyntax

//  private case class BufferImpl[A](dataset: Queue[(A, Count)], size: Int) extends Buffer[A]

//  def apply[A](dataset: Queue[(A, Count)], count: Int): Buffer[A] = bare(dataset, count)

  def apply[A](dataset: List[(A, Count)]): Buffer[A] = Buffer(Queue(dataset: _*), dataset.size)

//  def bare[A](dataset: Queue[(A, Count)], count: Int): Buffer[A] = BufferImpl(dataset, count)

  def empty[A]: Buffer[A] = Buffer(Queue.empty[(A, Count)], 0)

}
