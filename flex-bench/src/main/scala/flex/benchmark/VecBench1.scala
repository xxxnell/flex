package flex.benchmark

import java.util.concurrent.TimeUnit

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

  @Param(Array("1", "2", "8734", "921600"))
  var dim: Int = _

  // variables

  var x: INDArray = _

  var xt: INDArray = _

  var a1: INDArray = _

  var a100: INDArray = _

  val scalar: INDArray = Nd4j.ones(1)

  @Setup
  def setup(): Unit = {
    val (h1, h2) = (1, 100)

    x = Nd4j.ones(dim)
    xt = x.transpose()
    a1 = Nd4j.ones(dim * h1).reshape(h1, dim)
    a100 = Nd4j.ones(dim * h2).reshape(h2, dim)

//    NativeOpsHolder.getInstance.getDeviceNativeOps.setElementThreshold(16384)
//    NativeOpsHolder.getInstance.getDeviceNativeOps.setTADThreshold(64)
  }

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
  def mmul100: INDArray =
    a100.mmul(xt)

  @Benchmark
  def mmul100Get: Double =
    a100.mmul(xt).getDouble(0L)

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
