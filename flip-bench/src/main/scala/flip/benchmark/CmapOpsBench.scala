package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip.cmap.Cmap

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class CmapOpsBench {

  @Param(Array("5000", "20000"))
  var caSize: Int = _

  val cmap = Cmap.uniform(caSize)

  @Benchmark
  def apply = {
    val i = new Random().nextDouble()
    cmap.apply(i)
  }

  @Benchmark
  def bin = {
    cmap.bin
  }

  @Benchmark
  def size = {
    cmap.size
  }

  @Benchmark
  def range = {
    val i = new Random().nextInt()
    cmap.range(i)
  }

}
