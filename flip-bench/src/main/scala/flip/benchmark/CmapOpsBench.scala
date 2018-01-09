package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip.cmap.{Cmap, UniformCmap}
import flip.hmap.HDim
import flip.range

import scala.util.Random
import flip.range.syntax._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class CmapOpsBench {

  @Param(Array("200", "20000"))
  var cmapSize: Int = _

  var cmap: Cmap = _

  @Setup
  def setupCmap(): Unit = {
    cmap = Cmap.uniform(cmapSize)
  }

  @Benchmark
  def apply: HDim = {
    val i = 1
    cmap.apply(i)
  }

  @Benchmark
  def bin: List[RangeP] = {
    cmap.bin
  }

  @Benchmark
  def size: HDim = {
    cmap.size
  }

  @Benchmark
  def range: RangeP = {
    val i = 2
    cmap.range(i)
  }

}
