package flex.benchmark.ops

import java.io.{File, FileOutputStream}

import com.github.nscala_time.time.Imports._
import org.openjdk.jmh.infra.BenchmarkParams
import org.openjdk.jmh.results.{RunResult, Result => BenchResult}

import scala.collection.JavaConverters._

object BenchOutOps {

  def write(path: String, results: Iterable[RunResult]): Unit = {
    def createFs: FileOutputStream = {
      val date = DateTime.now
      val f = new File(s"$path/benchmark-$date.out")
      f.getParentFile.mkdirs()
      new FileOutputStream(f)
    }

    def writeRes(fs: FileOutputStream): Unit = {
      def records2b(records: List[String]): Array[Byte] = (records.mkString(", ") + "\n").getBytes("UTF-8")

      fs.write(records2b(RecordOps.recordLabels))
      results.foreach { result: RunResult =>
        fs.write(records2b(RecordOps.records(result)))
      }
    }

    val fs = createFs
    writeRes(fs)
    fs.close()
  }

  object RecordOps {

    def records(result: RunResult): List[String] =
      metadata2Records(result.getAggregatedResult.getParams) :::
        result2Records(result.getAggregatedResult.getPrimaryResult)

    def recordLabels: List[String] =
      paramLabels :::
        resultLabels

    def metadata2Records(params: BenchmarkParams): List[String] =
      params.getBenchmark ::
        metadata2Params(params).mkString(" & ") ::
        Nil

    def metadata2Params(params: BenchmarkParams): Map[String, String] =
      params.getParamsKeys.asScala.toList
        .map(key => {
          (key, params.getParam(key))
        })
        .toMap

    def paramLabels: List[String] =
      "Benchmark" ::
        "Parameters" ::
        Nil

    def result2Records(result: BenchResult[_]): List[String] =
      result.getScore.toString ::
        result.getScoreError.toString ::
        result.getScoreUnit ::
        result.getSampleCount.toString ::
        Nil

    def resultLabels: List[String] = "Score" :: "Error" :: "Unit" :: "Count" :: Nil

  }

}
