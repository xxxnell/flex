package sketch.scope.experiment.ops

import java.io.{File, FileOutputStream}

import sketch.scope.plot.Plot

import scala.util.Try

object ExpOutOps {

  val defaultPath = "scope-sketch-bench/experiments"

  // syntax

  def clear(name: String): Unit = clear(defaultPath, name)

  def writePlot(name: String, affix: String, plot: Plot): Unit =
    writePlot(defaultPath, name, affix, plot)

  def writePlots(name: String, plots: List[(Int, Plot)]): Unit =
    writePlotsForDetails(defaultPath, name, plots)

  def writePlots(name: String, subname: String, plots: List[(Int, Plot)]): Unit =
    writePlotsForDetailsSubname(defaultPath, name, subname, plots)

  // ops

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

  def writePlotsForDetails(path: String, name: String, plots: List[(Int, Plot)]): Unit = {
    for (idxPlot <- plots) {
      val (idx, plot) = idxPlot
      writePlot(path, name, s"$idx", plot)
    }
  }

  def writePlotsForDetailsSubname(path: String, name: String, subname: String, plots: List[(Int, Plot)]): Unit = {
    for (idxPlot <- plots) {
      val (idx, plot) = idxPlot
      writePlot(path, name, s"$subname-$idx", plot)
    }
  }


}
