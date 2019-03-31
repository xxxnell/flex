package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.benchmark.complex.FCMNISTNet
import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.pdf.VQH
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.vec._
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ComplexBench1 {

  var complex: Complex = _

  var input: Vec = _

  var inputs: Seq[Vec] = _

  var output: Vec = _

  @Setup
  def setup(): Unit = {
    complex = FCMNISTNet.complex
    input = Vec.ones(complex.in.dims.head)
    inputs = Vec.stds(List.fill(1000)(complex.in.dims.head), IRng(0))._1
    output = Vec.ones(complex.out.dims.head)
  }

  @Benchmark
  def update1: Complex = {
    complex.clear
    complex.update(input)
  }

  @Benchmark
  def update1000: Complex = {
    complex.clear
    complex.update(inputs: _*)
  }

  @Benchmark
  def poolUpdate: (VQH, List[SumVec], List[SumVec]) = {
    complex.clear
    complex.pools.head.expUpdate((SumVec(input), 1.0f) :: Nil)
  }

  @Benchmark
  def inUpdate: (VQH, List[SumVec], List[SumVec]) = {
    complex.clear
    complex.in.parUpdate((input, 0, 1.0f) :: Nil, { case (_, _, a) => a })
  }

  @Benchmark
  def nn: Float =
    FCMNISTNet.nn(complex.in.last).headOption.map(_.getFloat(0L)).getOrElse(0.0f)

  @Benchmark
  def inSearch: Option[SumVec] = {
    complex.clear
    complex.in.parSearch(input, 0)
  }

  @Benchmark
  def outUpdate: (VQH, List[SumVec], List[SumVec]) = {
    complex.clear
    complex.out.parUpdate((output, 0, 1.0f) :: Nil, { case (_, _, a) => a })
  }

}
