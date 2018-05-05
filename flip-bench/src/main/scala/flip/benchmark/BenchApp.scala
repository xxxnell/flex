package flip.benchmark

import flip.benchmark.ops.BenchOutOps
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options._

import scala.collection.JavaConverters._
import scala.math._

object BenchApp {

  def main(args: Array[String]): Unit = {

    // confs
    val opts0 = BenchAppConfs.envOptions(args.toList)
    val opts1 = BenchAppConfs.expIterateSize(opts0, 1, 1000, 2)

    // run
    val results = new Runner(opts1.build()).run().asScala

    // results
    val path = "benchmarks"
    BenchOutOps.write(path, results)
  }

}

object BenchAppConfs {

  val warmup = 20
  val measurement = 30
  val thread = 1
  val fork = 1

  def envOptions(args: List[String]): ChainedOptionsBuilder = {
    val includeO: Option[String] = args.headOption

    val builder0 = new OptionsBuilder()
    val builder1 = builder0.warmupIterations(BenchAppConfs.warmup)
    val builder2 = builder1.measurementIterations(BenchAppConfs.measurement)
    val builder3 = builder2.threads(BenchAppConfs.thread)
    val builder4 = builder3.forks(BenchAppConfs.fork)
    val builder5 = includeO.fold(builder4)(include => builder4.include(include))

    builder5
  }

  def linearIterateSize(builder: ChainedOptionsBuilder, start: Int, end: Int, by: Int): ChainedOptionsBuilder =
    builder.param("iterateBenchSize", (start to end by by).map(_.toString).toArray: _*)

  def expIterateSize(builder: ChainedOptionsBuilder, start: Int, end: Int, base: Double): ChainedOptionsBuilder = {
    builder.param(
      "iterateBenchSize",
      (0 to (log(end / start) / log(base)).toInt).map(a => (start * pow(base, a).toInt).toString).toArray: _*)
  }

}
