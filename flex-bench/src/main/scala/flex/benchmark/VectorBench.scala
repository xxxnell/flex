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
class VectorBench {

  // parameters

  @Param(Array("1", "2", "128", "1024"))
  var dim: Int = _

  var x: Vector[Int] = _

  @Setup
  def setup(): Unit =
    x = Vector(1 to dim: _*)

  @Benchmark
  def updated: Vector[Int] =
    x.updated(dim - 1, dim)

  @Benchmark
  def indexOf: Int =
    x.indexOf(dim)

  @Benchmark
  def apply: Int =
    x.apply(dim - 1)

  @Benchmark
  def remove1: Vector[Int] =
    x.patch(dim - 1, Nil, 1)

  @Benchmark
  def remove2: Vector[Int] =
    x.take(dim / 2) ++ x.takeRight(dim / 2)

}
