package flex.benchmark.complex

import java.util.concurrent.TimeUnit

import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.pdf.{NormalDist, VQH}
import flex.pdf.VQH.syntax._
import flex.util.RandomIdentitySet
import flex.vec.Vec
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class AddVarBench {

  @Param(Array("10000"))
  var l: Int = _

  @Param(Array("20"))
  var d: Int = _

  @Benchmark
  def addVar: Complex = Complex.empty(100, 10).addStd(List.fill(d)(l -> 10))

  @Benchmark
  def empty: Complex = Complex.empty(100, 10)

  @Benchmark
  def renewOut: Complex = Complex.renewOut(Complex.empty(100, 10))

  @Benchmark
  def addDim: VQH =
    List.fill(d)(l).foldLeft(Complex.empty(100, 10).in) {
      case (vqh, _l) => vqh.addDim(List.fill(_l)(NormalDist.std))
    }

}
