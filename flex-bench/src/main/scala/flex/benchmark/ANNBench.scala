package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.nns._
import flex.nns.ANN.syntax._
import flex.rand.IRng
import flex.vec.Vec
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import flex.util.IdentityHashMap
import flex.util.IdentityHashMap.syntax._
import flex.util.IdentityHashSet
import flex.util.IdentityHashSet.syntax._

import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ANNBench {

  @Param(Array("1"))
  var dim: Int = _

  @Param(Array("3"))
  var l: Int = _

  @Param(Array("10", "100"))
  var n: Int = _

  var ann: VecANN = _

  var x: Vec = _

  object SearchAlgorithm {

    def hashs: List[Int] = ann.lsh.hash(x)

    lazy val hashsC: List[Int] = hashs

    def vecs: List[Vec] =
      hashsC.zip(ann.htables).flatMap { case (h, ht) => ht.getOrElse(h, IdentityHashSet.empty[Vec]).toList }

    lazy val vecsC: List[Vec] = vecs

    def neighbors: List[Vec] =
      VecANN.frequentest(vecs)

    lazy val neighborsC: List[Vec] = neighbors

    def result: Option[Vec] =
      neighborsC.map(n => (n, VecANN.distance(x, n))).sortBy(_._2).headOption.map(_._1)

  }

  @Setup
  def setup(): Unit = {
    val rng0 = IRng(0)
    val (ann0, rng1) = VecANN.empty(l, dim, rng0)
    val (prepvs, _) = Vec.std(dim, rng1, n)

    x = Vec.ones(dim)
    ann = prepvs.foldLeft(ann0) { case (_ann, prepv) => _ann.add(prepv) }.add(x)
  }

  @Benchmark
  def add: VecANN = ann.add(x)

  @Benchmark
  def remove: VecANN = ann.remove(x)

  @Benchmark
  def search: Option[INDArray] = ann.search(x)

  @Benchmark
  def hashs: List[Int] = SearchAlgorithm.hashs

  @Benchmark
  def vecs: List[Vec] = SearchAlgorithm.vecs

  @Benchmark
  def neighbors: List[Vec] = SearchAlgorithm.neighbors

  @Benchmark
  def result: Option[Vec] = SearchAlgorithm.result

}
