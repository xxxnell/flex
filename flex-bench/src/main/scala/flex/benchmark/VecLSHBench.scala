package flex.benchmark

import flex.nns.ANN.syntax._
import flex.nns._
import flex.rand.IRng
import flex.vec.Vec
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class VecLSHBench {

  @Param(Array("1"))
  var dim: Int = _

  @Param(Array("3"))
  var l: Int = _

  @Param(Array("1"))
  var memoSize: Int = _

  var lsh: VecLSH = _

  var x: Vec = _

  @Setup
  def setup(): Unit = {
    val w = List.fill(l)(1.0f)
    lsh = VecLSH(dim, w, memoSize, IRng(0))._1
    x = Vec.std(dim, IRng(0))._1
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
