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
class LSHBench {

  @Param(Array("1", "50176"))
  var dim: Int = _

  @Param(Array("3"))
  var l: Int = _

  var lsh: VecLSH = _

  var x: Vec = _

  object HashsAlgorithm {

    def hashfs: Vec = VecLSH.hashf(lsh, x)

    lazy val hashfsC: Vec = hashfs

    def getFloat: Float = hashfsC.getFloat(0L)

    def toList: List[Int] = (0 until VecLSH.size(lsh)).toList.map(i => hashfsC.getFloat(i.toLong).floor.round)

  }

  @Setup
  def setup(): Unit = {
    val w = List.fill(l)(1.0f)
    lsh = VecLSH(dim, w, IRng(0))._1
    x = Vec.std(dim, IRng(0))._1
  }

  @Benchmark
  def hashList: List[Int] = VecLSH.hashList(lsh, x)

  @Benchmark
  def hashVec: Vec = VecLSH.hashVec(lsh, x)

  @Benchmark
  def hashVecGetInt: Int = VecLSH.hashVec(lsh, x).getInt(0)

  @Benchmark
  def hashfs: Vec = HashsAlgorithm.hashfs

  @Benchmark
  def getFloat: Float = HashsAlgorithm.getFloat

  @Benchmark
  def toList: List[Int] = HashsAlgorithm.toList

}
