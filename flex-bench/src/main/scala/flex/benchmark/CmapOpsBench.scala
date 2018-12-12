package flex.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flex.cmap.{Cmap, UniformCmap}
import flex.hmap.HDim
import flex.range

import scala.util.Random
import flex.range.syntax._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class CmapOpsBench {

  @Param(Array("20", "2000"))
  var cmapSize: Int = _

  var cmap: Cmap = _

  @Setup
  def setupCmap(): Unit = {
    cmap = Cmap.uniform(cmapSize)
  }

  @Benchmark
  def apply: HDim = {
    cmap.apply(cmapSize / 2)
  }

  @Benchmark
  def bins: List[RangeP] = {
    cmap.bins
  }

  @Benchmark
  def binsArr: Array[RangeP] = {
    cmap.binsArr
  }

  @Benchmark
  def size: HDim = {
    cmap.size
  }

  @Benchmark
  def range: RangeP = {
    cmap.range(cmapSize / 2)
  }

}
