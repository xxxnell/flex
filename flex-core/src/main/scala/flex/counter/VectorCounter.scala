package flex.counter

trait VectorCounter extends Counter {

  def counts: Vector[Double]

}

trait VectorCounterOps extends CounterOps[VectorCounter] {

  def update(counter: VectorCounter, cdim: CDim, count: Double): VectorCounter =
    if (cdim >= 0 && cdim < size(counter)) {
      VectorCounter(counter.counts.updated(cdim, counter.counts.apply(cdim) + count))
    } else throw new IndexOutOfBoundsException

}

object VectorCounter extends VectorCounterOps {

  private case class VectorCounterImpl(counts: Vector[Double]) extends VectorCounter

  def apply(counts: Vector[Double]): VectorCounter = VectorCounterImpl(counts)

  def empty(size: Int): VectorCounter = VectorCounter(Vector.fill(size)(0))

}
