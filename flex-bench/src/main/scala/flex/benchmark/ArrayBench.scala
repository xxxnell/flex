package flex.benchmark

import java.util.concurrent.TimeUnit

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.nativeblas.NativeOpsHolder
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ArrayBench {

  // parameters

//  @Param(Array("1", "2", "784", "921600"))
  @Param(Array("1"))
  var dim: Int = _

  // variables

  var rowNdarray: INDArray = _

  var colNdarray: INDArray = _

  @Setup
  def setup(): Unit = {
    rowNdarray = Nd4j.ones(dim)
    colNdarray = rowNdarray.transpose()

    NativeOpsHolder.getInstance.getDeviceNativeOps.setElementThreshold(16384)
    NativeOpsHolder.getInstance.getDeviceNativeOps.setTADThreshold(64)
  }

  @Benchmark
  def add: INDArray = {
    rowNdarray.add(1)
  }

  @Benchmark
  def mul: INDArray = {
    rowNdarray.mul(colNdarray)
  }

}
