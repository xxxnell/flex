package sketch.scope.counter

/**
  * Licensed by Probe Technology, Inc.
  */
trait ListCounter extends Counter {
  def cs: List[Double]
}

trait ListCounterOp extends CounterOps[ListCounter] {

  def update(counter: ListCounter, cdim: CDim, count: Double): Option[ListCounter] = {
    if(cdim < size(counter)) {
      Some(ListCounter(counter.cs.zipWithIndex.map { case (c, idx) => if(idx == cdim) c + count else c }))
    } else None
  }

}

object ListCounter extends ListCounterOp {

  private case class ListCounterImpl(cs: List[Double]) extends ListCounter

  def apply(cs: List[Double]): ListCounter = ListCounterImpl(cs)

  def empty(cdimSize: CDim): ListCounter = ListCounter(List.fill[Double](cdimSize)(0))

}