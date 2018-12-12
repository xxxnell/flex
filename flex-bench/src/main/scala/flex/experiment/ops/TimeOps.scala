package flex.experiment.ops

trait TimeOps { self =>

  def time[A](block: => A): (A, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val t = t1 - t0
    (result, t)
  }

  def printTime[A](block: => A): A = {
    val (result, time) = self.time(block)
    println(s"elapsed time: $time (ns)")
    result
  }

}

object TimeOps
