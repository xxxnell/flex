package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.rand.IRng
import flex.vec.SumVec
import flex.pdf.VQH
import flex.pdf.VQH.syntax._
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class VQHBench {

  @Param(Array("1024"))
  var dim1: Int = _

  @Param(Array("2048"))
  var dim2: Int = _

  @Param(Array("5", "100"))
  var k: Int = _

  var vqh: VQH = _

  var input: SumVec = _

  var inputs: List[SumVec] = _

  @Setup
  def setup(): Unit = {
    val dims = (dim1 :: dim2 :: Nil).filter(dim => dim != 0)
    inputs = SumVec.stds(dims, IRng(1), k * 10)._1
    input = inputs.head
    vqh = VQH.empty(dims, k).expUpdate(inputs.map(v => (v, 1.0f)))._1
  }

  @Benchmark
  def parUpdate: (VQH, List[SumVec], List[SumVec]) = {
    vqh.clear
    vqh.parUpdate((input.head, 0, 0f) :: Nil, { case (_, _, sv) => sv })
  }

  @Benchmark
  def expUpdate: (VQH, List[SumVec], List[SumVec]) = {
    vqh.clear
    vqh.expUpdate((input, 0f) :: Nil)
  }

  @Benchmark
  def parSearch: Option[SumVec] = {
    vqh.clear
    vqh.parSearch(input.head, 0)
  }

  @Benchmark
  def expSearch: Option[SumVec] = {
    vqh.clear
    vqh.expSearch(input)
  }

  @Benchmark
  def singleUpdate: (VQH, List[SumVec], List[SumVec]) = {
    vqh.clear
    VQH.singleUpdate(vqh, input, input, 1.0f)
  }

  @Benchmark
  def addStd: VQH =
    vqh.addDimStd(10000)

}
