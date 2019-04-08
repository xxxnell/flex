package flex.benchmark.complex

import java.util.concurrent.TimeUnit

import flex.LeNet
import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.vec.Vec
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class LeNetBench2 {

  @Param(Array("50"))
  var n: Int = _

  var complex: Complex = _

  var dats: List[Vec] = _

  @Setup
  def setup: Unit = {
    complex = LeNet.complex
    dats = Vec.stds(complex.in.dims.head, IRng(0), n)._1
  }

  @Benchmark
  def update: Complex = {
    complex.clear
    complex.update(dats: _*)
  }

}
