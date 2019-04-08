package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.rand.IRng
import flex.vec.Vec
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.indexing.NDArrayIndex
import org.nd4j.nativeblas.NativeOpsHolder
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class VecBench1 {

  // parameters

  @Param(Array("247678", "2476788", "24767882"))
  var dim: Long = _

  // variables

  var x: INDArray = _

  var xt: INDArray = _

  var a1: INDArray = _

  var aN: INDArray = _

  val scalar: INDArray = Nd4j.ones(1)

  @Setup
  def setup(): Unit = {
    val (h1, h2) = (1, 5)

    x = Nd4j.ones(dim)
    xt = x.transpose()
    a1 = Nd4j.ones(h1 * dim).reshape(h1, dim)
    aN = Nd4j.ones(h2 * dim).reshape(h2, dim)
  }

  @Benchmark
  def std: Vec = Vec.std(dim.toInt, IRng(0))._1

  @Benchmark
  def normal: Vec = Vec.normal(Vec.zeros(dim.toInt), Vec.ones(dim.toInt), IRng(0))._1

  @Benchmark
  def add: INDArray =
    x.add(1)

  @Benchmark
  def mmul1: INDArray =
    a1.mmul(xt)

  @Benchmark
  def mmul1Get: Double =
    a1.mmul(xt).getDouble(0L)

  @Benchmark
  def mmulN: INDArray =
    aN.mmul(xt)

  @Benchmark
  def mmulNGet: Double =
    aN.mmul(xt).getDouble(0L)

  @Benchmark
  def mmul100GetGet: Double = {
    val b = aN.mmul(xt)
    b.getDouble(0L) + b.getDouble(1L)
  }

  @Benchmark
  def mmul100GetMmul: Double = {
    val b = aN.mmul(xt)
    val v1 = b.getDouble(0L)
    val v2 = b.add(1.0).getDouble(0L)
    v1 + v2
  }

  @Benchmark
  def mmul100Add: INDArray =
    aN.mmul(xt).add(1.0)

  @Benchmark
  def getLast: INDArray =
    x.get(NDArrayIndex.indices(dim - 1))

  @Benchmark
  def concat: INDArray =
    Nd4j.concat(1, x, scalar)

  @Benchmark
  def equals: Boolean = x.equals(x)

  @Benchmark
  def eq: Boolean = x.eq(x)

  @Benchmark
  def vstack: INDArray = Nd4j.vstack(xt, xt)

  @Benchmark
  def get: Double = x.getDouble((dim / 2).toLong)

  @Benchmark
  def amin: Number = x.aminNumber()

  @Benchmark
  def dup: INDArray = x.dup()

}
