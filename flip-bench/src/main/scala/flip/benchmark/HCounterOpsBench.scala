package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip.hcounter.HCounter

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class HCounterOpsBench {

  @Param(Array("1", "30"))
  var counterNo: Int = _

  @Param(Array("1000", "100000"))
  var counterSize: Int = _

  val seed = 0
  var hcounter: HCounter = _

  @Setup
  def setupHCounter = {
    hcounter = HCounter.empty(counterNo, counterSize, seed)
  }

  @Benchmark
  def construct: HCounter = {
    HCounter.empty(counterNo, counterSize, seed)
  }

  @Benchmark
  def update: Option[HCounter] = {
    val dim = 1
    hcounter.update(dim, 1)
  }

  @Benchmark
  def get: Option[Double] = {
    val dim = 1
    hcounter.get(dim)
  }

  @Benchmark
  def sum: Double = {
    hcounter.sum
  }

  @Benchmark
  def count: Option[Double] = {
    val dim1 = 1
    val dim2 = 3
    val range = if(dim2 > dim1) (dim1, dim2) else (dim2, dim1)
    hcounter.count(range._1, range._2)
  }

}
