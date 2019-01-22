package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.nns._
import flex.nns.ANN.syntax._
import flex.rand.IRng
import flex.vec.Vec
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ANNBench {

  @Param(Array("1024", "131072"))
  var dim: Int = _

  var l: Int = 10

  var ndarrAnn: VecANN = _

  var v: INDArray = _

  @Setup
  def setup(): Unit = {
    val (n, rng0) = (100, IRng(0))
    val (ndarrAnn0, rng1) = VecANN.empty(l, dim, rng0)
    val (prepvs, rng2) = Vec.std(dim, rng1, n)

    v = Vec.ones(dim)
    ndarrAnn = prepvs.foldLeft(ndarrAnn0) { case (_ann, prepv) => _ann.add(prepv) }.add(v)
  }

  @Benchmark
  def add: VecANN =
    ndarrAnn.add(v)

  @Benchmark
  def remove: VecANN =
    ndarrAnn.remove(v)

  @Benchmark
  def search: Option[INDArray] =
    ndarrAnn.search(v)

}
