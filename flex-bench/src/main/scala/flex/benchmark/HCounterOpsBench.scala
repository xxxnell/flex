package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.counter.Counter
import org.openjdk.jmh.annotations._
import flex.hcounter.HCounter

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class HCounterOpsBench {

  @Param(Array("1"))
  var counterNo: Int = _

  @Param(Array("20", "2000"))
  var counterSize: Int = _

  val seed = 0

  var hcounter: HCounter = _

  @Setup
  def setupHCounter(): Unit =
    hcounter = HCounter.empty(counterNo, counterSize, seed)

  @Benchmark
  def construct: HCounter =
    HCounter.empty(counterNo, counterSize, seed)

  @Benchmark
  def update: HCounter = {
    val dim = counterSize / 2
    hcounter.update(dim, 1)
  }

  @Benchmark
  def updates: HCounter = {
    val dim = counterSize / 2
    val as = (dim, 1.0) :: Nil
    hcounter.updates(as)
  }

  @Benchmark
  def counterUpdate: Counter = {
    val dim = counterSize / 2
    hcounter.structures.head._2.update(dim, 1)
  }

  @Benchmark
  def get: Double = {
    val dim = counterSize / 2
    hcounter.get(dim)
  }

  @Benchmark
  def sum: Double =
    hcounter.sum

  @Benchmark
  def count: Double = {
    val (dim1, dim2) = (1, 3)
    hcounter.count(dim1, dim2)
  }

}
