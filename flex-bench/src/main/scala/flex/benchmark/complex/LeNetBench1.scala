package flex.benchmark.complex

import java.util.concurrent.TimeUnit

import flex.LeNet
import flex.vec._
import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.pdf.VQH
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.vec.{SumVec, Vec}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class LeNetBench1 {

  var complex: Complex = _

  var input: SumVec = _

  var dat: Vec = _

  var output: SumVec = _

  @Setup
  def setup: Unit = {
    complex = LeNet.complex
    input = SumVec.std(complex.in.dims, IRng(0))._1
    dat = input.head
    output = SumVec.std(complex.out.dims, IRng(0))._1
  }

  @Benchmark
  def construct: Complex = LeNet.complex

  @Benchmark
  def update: Complex = {
    complex.clear
    complex.update(dat)
  }

  @Benchmark
  def nn: SumVec = LeNet.nn(input)

  @Benchmark
  def inUpdate: (VQH, List[SumVec], List[SumVec]) = {
    complex.clear
    complex.in.parUpdate((input.head, 0, 1.0f) :: Nil, { case (_, _, a) => a })
  }

  @Benchmark
  def outUpdate: (VQH, List[SumVec], List[SumVec]) = {
    complex.clear
    complex.out.parUpdate((output.head, 0, 1.0f) :: Nil, { case (_, _, a) => a })
  }

}
