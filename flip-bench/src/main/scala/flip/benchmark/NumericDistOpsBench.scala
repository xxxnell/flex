package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip._
import flip.pdf.SmoothDist

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class NumericDistOpsBench {

  @Benchmark
  def sample: (SmoothDist[Double], Double) = {
    NumericDist.normal(0.0, 1.0).sample
  }

}
