package sketch.scope

import java.io.{File, FileOutputStream}

import com.github.nscala_time.time.Imports._
import org.openjdk.jmh.infra.BenchmarkParams
import org.openjdk.jmh.results.{Result => BenchResult, RunResult}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options._

import scala.collection.JavaConverters._

/**
  * Licensed by Probe Technology, Inc.
  */
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

object BenchOutOps {

  def write(path: String, results: Iterable[RunResult]): Unit = {
    def createFs: FileOutputStream = {
      val date = DateTime.now
      val f = new File(s"$path/benchmark-$date.out")
      f.getParentFile.mkdirs()
      new FileOutputStream(f)
    }

    def writeRes(fs: FileOutputStream): Unit = {
      def records2b(records: List[String]): Array[Byte] = (records.mkString("\t") + "\n").getBytes("UTF-8")

      fs.write(records2b(RecordOps.recordLabels))
      results.foreach { result: RunResult => fs.write(records2b(RecordOps.records(result))) }
    }

    val fs = createFs
    writeRes(fs)
    fs.close()
  }

  object RecordOps {

    def records(result: RunResult): List[String] = {
      metadata2Records(result.getAggregatedResult.getParams) :::
        result2Records(result.getAggregatedResult.getPrimaryResult)
    }

    def recordLabels: List[String] = {
      paramLabels :::
        resultLabels
    }

    def metadata2Records(params: BenchmarkParams): List[String] = {
      params.getBenchmark ::
        metadata2Params(params).mkString(", ") ::
        Nil
    }

    def metadata2Params(params: BenchmarkParams): Map[String, String] = {
      params.getParamsKeys.asScala.toList.map(key => {
        (key, params.getParam(key))
      }).toMap
    }

    def paramLabels: List[String] = {
      "Benchmark" ::
        "Parameters" ::
        Nil
    }

    def result2Records(result: BenchResult[_]): List[String] = {
      result.getScore.toString ::
        result.getScoreError.toString ::
        result.getScoreUnit ::
        result.getSampleCount.toString ::
        Nil
    }

    def resultLabels: List[String] = "Score" :: "Error" :: "Unit" :: "Count" :: Nil

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