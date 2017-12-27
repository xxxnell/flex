package sketch.scope.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import sketch.scope.hcounter.HCounter

import scala.util.Random

/**
  * Licensed by Probe Technology, Inc.
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class HCounterOpsBench {

  @Param(Array("1", "30"))
  var coDepth: Int = _

  @Param(Array("1000", "100000"))
  var coSize: Int = _

  val seed = 0
  val hcounter = HCounter.empty(coDepth, coSize, seed)

  @Benchmark
  def construct = {
    HCounter.empty(coDepth, coSize, seed)
  }

  @Benchmark
  def update = {
    val dim = new Random().nextInt() % coSize
    hcounter.update(dim, 1)
  }

  @Benchmark
  def get = {
    val dim = new Random().nextInt() % coSize
    hcounter.get(dim)
  }

  @Benchmark
  def sum = {
    hcounter.sum
  }

  @Benchmark
  def count = {
    val dim1 = new Random().nextInt() % coSize
    val dim2 = new Random().nextInt() % coSize
    val range = if(dim2 > dim1) (dim1, dim2) else (dim2, dim1)
    hcounter.count(range._1, range._2)
  }

}
