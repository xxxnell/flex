package flex.counter

import scala.util.Try

/**
 * Counter for memorizing counts.
 */
trait Counter {

  def counts: Seq[Double]

  override def toString: String = {
    val counterStr = if (counts.size < 100) s"(${counts.mkString(", ")})" else s"(|counts| = ${counts.size})"
    s"Counter($counterStr)"
  }

}

trait CounterOps[C <: Counter] extends CounterLaws[C] {

  def update(counter: C, cdim: CDim, count: Double): C

}

trait CounterLaws[C <: Counter] { self: CounterOps[C] =>

  def get(counter: C, cdim: CDim): Double = counter.counts.apply(cdim)

  def size(counter: C): Int = counter.counts.size

}

trait CounterSyntax {

  implicit class CounterSyntaxImpl(counter: Counter) {
    def update(cdim: CDim, count: Double): Counter = Counter.update(counter, cdim, count)
    def get(cdim: CDim): Double = Counter.get(counter, cdim)
    def size: Int = Counter.size(counter)
  }

}

object Counter extends CounterOps[Counter] {

  def empty(size: Int): VectorCounter = VectorCounter.empty(size)

  def update(counter: Counter, cdim: CDim, count: Double): Counter = counter match {
    case counter: ListCounter   => ListCounter.update(counter, cdim, count)
    case counter: VectorCounter => VectorCounter.update(counter, cdim, count)
  }

}
