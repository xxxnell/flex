package flip.counter

trait ListCounter extends Counter {
  def counts: List[Double]
}

trait ListCounterOp extends CounterOps[ListCounter] {

  def update(counter: ListCounter, cdim: CDim, count: Double): Option[ListCounter] = {
    if (cdim >= 0 && cdim < size(counter)) {
      Some(ListCounter(counter.counts.updated(cdim, counter.counts.apply(cdim) + count)))
    } else None
  }

}

object ListCounter extends ListCounterOp {

  private case class ListCounterImpl(counts: List[Double]) extends ListCounter

  def apply(cs: List[Double]): ListCounter = ListCounterImpl(cs)

  def empty(size: Int): ListCounter = ListCounter(List.fill[Double](size)(0))

}
