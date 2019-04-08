package flex.benchmark.complex

import java.util.concurrent.TimeUnit

import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.pdf.{NormalDist, VQH}
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.util.RandomIdentitySet
import flex.vec.{SumVec, Vec}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class AddVarBench {

  @Param(Array("50000"))
  var l: Int = _

  @Param(Array("10"))
  var d: Int = _

  var k: Int = 10

  @Benchmark
  def addVar: Complex = Complex.empty(100, 10).addDimStd(List.fill(d)(l -> k))

  @Benchmark
  def empty: Complex = Complex.empty(100, 10)

  @Benchmark
  def renewOut: Complex = Complex.renewOut(Complex.empty(100, 10))

  @Benchmark
  def inAddDimStd: VQH = Complex.empty(100, 10).in.addDimStd(List.fill(d)(l))

  @Benchmark
  def vecStd: SumVec = Vec.stds(List.fill(d)(l), IRng(0))._1

}
