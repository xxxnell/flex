package sketch.scope

import java.io.{File, FileOutputStream}

import com.github.nscala_time.time.Imports.DateTime
import sketch.scope.experiment.BasicExp.expName
import sketch.scope.plot.Plot

import scala.util.Try

/**
  * Licensed by Probe Technology, Inc.
  */
object ExpOutOps {

  val defaultPath = "experiments"

  def clear(name: String): Unit = clear(defaultPath, name)

  def writePlot(name: String, affix: String, plot: Plot): Unit = writePlot(defaultPath, name, affix, plot)

  def writePlots(name: String, plots: List[Plot]): Unit = writePlots(defaultPath, name, plots)

  def clear(path: String, name: String): Unit = Try {
    new File(s"$path/$name").listFiles.foreach(file => file.delete())
    new File(s"$path/$name").delete()
  }.getOrElse(())

  def writePlot(path: String, name: String, affix: String, plot: Plot): Unit = {
    // create fs
    val f = new File(s"$path/$name/$name-$affix.out")
    f.getParentFile.getParentFile.mkdirs()
    f.getParentFile.mkdirs()
    val fs = new FileOutputStream(f)

    // plot -> str
    val records = plot.records.map { case (range, value) => range.start :: range.end :: value :: Nil }
    val recordsStr = records.map(_.mkString(", ")).mkString("\n")

    // write
    fs.write(recordsStr.getBytes("UTF-8"))
    fs.close()
  }

  def writePlots(path: String, name: String, plots: List[Plot]): Unit = {
    for (plotIdx <- plots.zipWithIndex) {
      val (plot, idx) = plotIdx
      writePlot(expName, idx.toString, plot)
    }
  }

}
