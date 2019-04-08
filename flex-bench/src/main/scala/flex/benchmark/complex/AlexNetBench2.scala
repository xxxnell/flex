package flex.benchmark.complex

import java.util.concurrent.TimeUnit

import flex.AlexNet
import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.vec.Vec
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class AlexNetBench2 {

  var complex: Complex = _

  @Param(Array("1000"))
  var n: Int = _

  var dats: List[Vec] = _

  @Setup
  def setup(): Unit = {
    complex = AlexNet.complex
    dats = Vec.stds(complex.in.dims.head, IRng(0), n)._1
  }

  @Benchmark
  def updateN: Complex = {
    complex.clear
    complex.update(dats: _*)
  }

}
