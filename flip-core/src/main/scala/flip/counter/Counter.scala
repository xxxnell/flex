package flip.counter

import scala.util.Try

/**
  * Counter for memorizing counts.
  */
trait Counter {

  def cs: List[Double]

}

trait CounterOps[C<:Counter] extends CounterLaws[C] {

  def update(counter: C, cdim: CDim, count: Double): Option[C]

}

trait CounterLaws[C<:Counter] { self: CounterOps[C] =>

  def get(counter: C, cdim: CDim): Option[Double] = Try (counter.cs.apply(cdim)).toOption

  def size(counter: C): Int = counter.cs.size

}

trait CounterSyntax {

  implicit class CounterSyntaxImpl(counter: Counter) {
    def update(cdim: CDim, count: Double): Option[Counter] = Counter.update(counter, cdim, count)
    def get(cdim: CDim): Option[Double] = Counter.get(counter, cdim)
    def size: Int = Counter.size(counter)
  }

}

object Counter extends CounterOps[Counter] {

  def empty(size: Int): ListCounter = ListCounter.empty(size)

  def update(counter: Counter, cdim: CDim, count: Double): Option[Counter] = counter match {
    case counter: ListCounter => ListCounter.update(counter, cdim, count)
    case _ => None
  }

}
