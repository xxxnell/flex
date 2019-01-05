package flex.benchmark.sketch

import java.util.concurrent.TimeUnit

import flex.pdf.{Buffer, Count}
import flex.pdf.Buffer.syntax._
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class BufferBench { self =>

  @Param(Array("1", "50", "5000"))
  var size: Int = _

  var buffer: Buffer[Double] = _

  var data1: List[(Double, Count)] = List.fill(1)((2.0, 2.0))

  var data100: List[(Double, Count)] = List.fill(100)((2.0, 2.0))

  @Setup
  def setupBuffer(): Unit =
    self.buffer = Buffer(List.fill(size)((1, 1)))

  @Benchmark
  def construct: Buffer[Double] =
    Buffer.empty[Double]

  @Benchmark
  def append: Buffer[Double] =
    buffer :+ (1, 1)

  @Benchmark
  def appends1: Buffer[Double] =
    buffer ++ data1

  @Benchmark
  def appends100: Buffer[Double] =
    buffer ++ data100

  @Benchmark
  def splitAt0: (Buffer[Double], Buffer[Double]) =
    buffer.splitAt(0)

  @Benchmark
  def splitAt1: (Buffer[Double], Buffer[Double]) =
    buffer.splitAt(1)

  @Benchmark
  def splitAt10: (Buffer[Double], Buffer[Double]) =
    buffer.splitAt(10)

  @Benchmark
  def toList: List[(Double, Count)] =
    buffer.toList

  @Benchmark
  def sum: Count =
    buffer.sum

}
