package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.nns._
import flex.rand.IRng
import flex.vec.{SumVec, Vec}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SumVecLSHBench {

  @Param(Array("1"))
  var dim: Int = _

  @Param(Array("10"))
  var depth: Int = _

  @Param(Array("3"))
  var l: Int = _

  @Param(Array("10"))
  var memoSize: Int = _

  var lsh: SumVecLSH = _

  var x: SumVec = _

  @Setup
  def setup(): Unit = {
    val dims = List.fill(depth)(dim)
    val w = List.fill(l)(1.0f)
    lsh = SumVecLSH(dims, w, memoSize, IRng(0))._1
    x = SumVec.std(dims, IRng(0))._1
  }

  @Benchmark
  def hash1: List[Int] = {
    lsh.clear
    lsh.hash(x)
  }

  @Benchmark
  def hash2: Boolean = {
    lsh.clear
    val hashs1 = lsh.hash(x)
    val hashs2 = lsh.hash(x)
    hashs1 == hashs2
  }

}
