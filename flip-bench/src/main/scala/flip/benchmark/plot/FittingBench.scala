package flip.benchmark.plot

import java.util.concurrent.TimeUnit

import flip.plot.Fitting
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class FittingBench {

  val as: List[(Double, Double)] = (1.0, 1.0) :: (2.0, 2.0) :: Nil

  @Setup
  def setup(): Unit = {}

  @Benchmark
  def simpleFitting(): Double = {
    val x = 1.5
    as match {
      case (x1, y1) :: (x2, y2) :: Nil =>
        val slope = (y2 - y1) / (x2 - x1)
        val c = y1 - slope * x1
        val interp = slope * x + c
        interp
      case _ => throw new Exception
    }
  }

  @Benchmark
  def dataFitting: Option[Double] = {
    Fitting.dataFitting(as, 1.5)
  }

}
