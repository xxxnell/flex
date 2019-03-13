package flex.benchmark

import flex.pdf.VQH.syntax._
import flex.vec._
import flex.chain.Complex
import flex.chain.Complex.syntax._

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import java.util.concurrent.TimeUnit

import flex.pdf.VQH
import flex.rand.IRng
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ComplexBench1 {

  var complex: Complex = _

  var input: Vec = _

  var inputs: Seq[Vec] = _

  var output: Vec = _

  val (kin, kout) = (20, 10)

  val (l0, l1, l2, l3) = (28 * 28, 10, 10, 1)

  val (k0, k1, k2, k3) = (20, 20, 20, 20)

  def nn(v0: SumVec): SumVec =
    ((v0 match {
      case x1 :: z1 :: rem => z1.reshape(l1, l0).mmul(x1).tanh :: rem
    }) match {
      case h1 :: z2 :: rem => z2.reshape(l2, l1).mmul(h1).tanh :: rem
    }) match {
      case h2 :: z3 :: rem => z3.reshape(l3, l2).mmul(h2) :: rem
    }

  @Setup
  def setup(): Unit = {
    complex = Complex
      .empty(kin, kout)
      .addStd(l0 -> k0, l0 * l1 -> k1, l1 * l2 -> k2, l2 * l3 -> k3)
      .map(v0 => nn(v0))
    input = Vec.ones(l0)
    inputs = Vec.stds(l0, IRng(0), 1000)._1
    output = Vec.ones(l3)
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
  def op: Float =
    nn(complex.in.last).headOption.map(_.getFloat(0L)).getOrElse(0.0f)

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
