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
class ArrayBench2 {

  // parameters

  var dim: Int = 16384

  // variables

  var x: INDArray = _

  @Setup
  def setup(): Unit =
    x = Nd4j.ones(dim)

//    NativeOpsHolder.getInstance.getDeviceNativeOps.setElementThreshold(16384)
//    NativeOpsHolder.getInstance.getDeviceNativeOps.setTADThreshold(64)

  @Benchmark
  def getInterval: INDArray =
    x.get(NDArrayIndex.interval(1000, 2000))

  @Benchmark
  def concat: INDArray =
    Nd4j.concat(1, x, x)

  @Benchmark
  def reshape: INDArray =
    x.reshape(128, 128)

}
