package flip.benchmark

import flip.Params
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
    val opts2 = BenchAppConfs.params(opts1)

    // run
    val results = new Runner(opts2.build()).run().asScala

    // results
    val path = "benchmarks"
    BenchOutOps.write(path, results)
  }

}

object BenchAppConfs {

  val warmup = 20
  val warmupTime: TimeValue = TimeValue.seconds(1)
  val measurement = 30
  val measurementTime: TimeValue = TimeValue.seconds(1)
  val thread = 1
  val fork = 1

  def envOptions(args: List[String]): ChainedOptionsBuilder = {
    val includeO: Option[String] = args.headOption

    val builder0 = new OptionsBuilder()
    val builder1 = builder0.warmupIterations(BenchAppConfs.warmup)
    val builder2 = builder1.warmupTime(BenchAppConfs.warmupTime)
    val builder3 = builder2.measurementIterations(BenchAppConfs.measurement)
    val builder4 = builder3.measurementTime(BenchAppConfs.measurementTime)
    val builder5 = builder4.threads(BenchAppConfs.thread)
    val builder6 = builder5.forks(BenchAppConfs.fork)
    val builder7 = includeO.fold(builder6)(include => builder4.include(include))

    builder7
  }

  def linearIterateSize(builder: ChainedOptionsBuilder, start: Int, end: Int, by: Int): ChainedOptionsBuilder =
    builder.param("iterateBenchSize", (start to end by by).map(_.toString).toArray: _*)

  def expIterateSize(builder: ChainedOptionsBuilder, start: Int, end: Int, base: Double): ChainedOptionsBuilder = {
    builder.param(
      "iterateBenchSize",
      (0 to (log(end / start) / log(base)).toInt).map(a => (start * pow(base, a).toInt).toString).toArray: _*)
  }

  def params(builder: ChainedOptionsBuilder): ChainedOptionsBuilder = {
    val params = Params.reducedBy(1)
    builder.param("cmapSizeLS", params.cmapSizes.map(_.toString).toArray: _*)
    builder.param("cmapNoLS", params.cmapNos.map(_.toString).toArray: _*)
    builder.param("bufferSizeLS", params.bufferSizes.map(_.toString).toArray: _*)
    builder.param("decayFactorLS", params.decayFactors.map(_.toString).toArray: _*)
    builder.param("rebuildThresholdLS", params.rebuildThresholds.map(_.toString).toArray: _*)
  }

}
