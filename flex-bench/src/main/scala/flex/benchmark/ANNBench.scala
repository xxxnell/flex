package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.nns.NDArrayANN
import flex.nns.NDArrayANN.syntax._
import flex.rand.IRng
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ANNBench {

  @Param(Array("1024"))
  var dim: Int = _

  var l: Int = 10

  var ndarrAnn: NDArrayANN = _

  var v: INDArray = _

  @Setup
  def setup(): Unit = {
    val n = 100
    val (ndarrAnn0, _) = NDArrayANN.empty(l, dim, IRng(0))

    v = Nd4j.ones(dim)
    ndarrAnn = (1 to n).foldLeft(ndarrAnn0) { case (_ann, _) => _ann.add(Nd4j.randn(1, dim)) }.add(v)
  }

  @Benchmark
  def add: NDArrayANN =
    ndarrAnn.add(v)

  @Benchmark
  def remove: NDArrayANN =
    ndarrAnn.remove(v)

  @Benchmark
  def search: Option[INDArray] =
    ndarrAnn.search(v)

}
