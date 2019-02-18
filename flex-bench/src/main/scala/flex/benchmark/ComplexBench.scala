package flex.benchmark

import flex.pdf.VQH.syntax._
import flex.vec._
import flex.chain.Complex
import flex.chain.Complex.syntax._

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import java.util.concurrent.TimeUnit

import flex.pdf.VQH
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ComplexBench {

  var complex: Complex = _

  var input: Vec = _

  @Setup
  def setup(): Unit = {
    val mnist = Dataset.mnistTest.runSyncUnsafe(10.seconds)
    val (kin, kout) = (20, 10)
    val (l0, l1, l2, l3) = (784, 10, 10, 1)
    val (k0, k1, k2, k3) = (20, 20, 20, 20)

    complex = Complex
      .empty(kin, kout)
      .addStd(l0 -> k0, l0 * l1 -> k1, l1 * l2 -> k2, l2 * l3 -> k3)
      .map { case x1 :: z1 :: rem => z1.reshape(l1, l0).mmul(x1).tanh :: rem }
      .map { case h1 :: z2 :: rem => z2.reshape(l2, l1).mmul(h1).tanh :: rem }
      .map { case h2 :: z3 :: rem => z3.reshape(l3, l2).mmul(h2) :: rem }
    input = mnist.in.head
  }

  @Benchmark
  def update: Complex =
    complex.update(input)

  @Benchmark
  def inUpdate: (VQH, List[SumVec], List[SumVec]) =
    complex.in.parUpdate((input, 0, 1.0f) :: Nil, { case (_, _, a) => a })

}
