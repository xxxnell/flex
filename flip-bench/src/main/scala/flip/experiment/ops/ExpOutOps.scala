package flip.experiment.ops

import java.io.{File, FileOutputStream}

import flip.plot.Plot

import scala.util.Try

object ExpOutOps {

  val defaultPath = "flip-bench/experiments"

  // syntax

  def clear(name: String): Unit = clear(defaultPath, name)

  def writeStr(name: String, affix: String, str: String): Unit =
    writeStr(defaultPath, name, affix, str)

  def writePlot(name: String, affix: String, plot: Plot): Unit =
    writePlot(defaultPath, name, affix, plot)

  @deprecated
  def writePlots(name: String, plots: List[(Int, Plot)]): Unit =
    writePlotsForDetails(defaultPath, name, plots)

  def writePlots(name: String, subname: String, plots: List[(Int, Plot)]): Unit =
    writePlotsForDetailsSubname(defaultPath, name, subname, plots)

  // ops

  def clear(path: String, name: String): Unit =
    Try {
      new File(s"$path/$name").listFiles.foreach(file => file.delete())
      new File(s"$path/$name").delete()
    }.getOrElse(())

  def writeStr(path: String, name: String, affix: String, c: String): Unit = {
    val f = new File(s"$path/$name/$name-$affix.out")
    f.getParentFile.getParentFile.mkdirs()
    f.getParentFile.mkdirs()
    val fs = new FileOutputStream(f)

    fs.write(c.getBytes("UTF-8"))
    fs.close()
  }

  def writePlot(path: String, name: String, affix: String, plot: Plot): Unit = {
    writeStr(path, name, affix, plot.csv)
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
