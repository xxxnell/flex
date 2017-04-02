package sketch.scope.counter

import sketch.scope.Result

/**
  * Licensed by Probe Technology, Inc.
  *
  * Counter for memorizing of counters.
  */
trait Counter {
  def cs: Seq[Int]
}

trait CounterOps extends CounterLaws {

  /**
    * @return left if cdim is out of bound
    * */
  def update(counter: Counter, cdim: CDim, count: Int): Result[Counter] = ???

  def get(counter: Counter, cdim: CDim): Result[Counter] = ???

  def size(counter: Counter): Int = ???

}

trait CounterLaws { self: CounterOps =>


}

trait CounterSyntax {



}

object Counter extends CounterOps {

  def apply(dim: Int): Counter = ???

}
