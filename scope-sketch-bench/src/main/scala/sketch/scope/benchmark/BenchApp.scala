package sketch.scope.benchmark

import org.openjdk.jmh.results.{Result => BenchResult}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options._
import sketch.scope.benchmark.ops.BenchOutOps

import scala.collection.JavaConverters._

object BenchApp {

  def main(args: Array[String]): Unit = {

    // confs
    val opts = BenchAppConfs.envOptions
      .param("iterateBenchSize", (0 to 0 by 10).map(_.toString).toArray: _*)
      .build()

    // run
    val results = new Runner(opts).run().asScala

    // results
    val path = "benchmarks"
    BenchOutOps.write(path, results)
  }

}

object BenchAppConfs {

  val warmup = 3
  val measurement = 5
  val thread = 1
  val fork = 1

  def envOptions: ChainedOptionsBuilder = {
    new OptionsBuilder()
      .warmupIterations(BenchAppConfs.warmup)
      .measurementIterations(BenchAppConfs.measurement)
      .threads(BenchAppConfs.thread)
      .forks(BenchAppConfs.fork)
  }

}