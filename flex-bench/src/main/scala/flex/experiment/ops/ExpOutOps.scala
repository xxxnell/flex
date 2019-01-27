package flex.experiment.ops

import java.io.{File, FileOutputStream}
import java.nio.file.Paths

import flex.plot.RangePlot

import scala.util.Try

object ExpOutOps {

  val defaultPath: String = (Paths.get(".").toString :: "experiments" :: Nil).mkString(File.separator)

  // syntax

  def clear(name: String): Unit = clear(defaultPath, name)

  def writeStr(name: String, affix: String, str: String): Unit =
    writeStr(defaultPath, name, affix, str)

  def writeStrs(name: String, subname: String, strs: List[(Int, String)]): Unit =
    writeStrs(defaultPath, name, subname, strs)

  @Deprecated
  def writePlot(name: String, affix: String, plot: RangePlot): Unit =
    writePlot(defaultPath, name, affix, plot)

  @Deprecated
  def writePlots(name: String, subname: String, plots: List[(Int, RangePlot)]): Unit =
    writePlotsForDetailsSubname(defaultPath, name, subname, plots)

  // ops

  def clear(path: String, name: String): Unit =
    Try {
      new File(s"$path/$name").listFiles.foreach(file => file.delete())
      new File(s"$path/$name").delete()
    }.getOrElse(())

  def writeStr(path: String, name: String, affix: String, str: String): Unit = {
    val f = new File(s"$path/$name/$name-$affix.out")
    f.getParentFile.getParentFile.mkdirs()
    f.getParentFile.mkdirs()
    val fs = new FileOutputStream(f)

    fs.write(str.getBytes("UTF-8"))
    fs.close()
  }

  def writeStrs(path: String, name: String, subname: String, strs: List[(Int, String)]): Unit =
    strs.foreach { case (idx, str) => writeStr(path, name, s"$subname-$idx", str) }

  @Deprecated
  def writePlot(path: String, name: String, affix: String, plot: RangePlot): Unit =
    writeStr(path, name, affix, plot.csv)

  @Deprecated
  def writePlotsForDetails(path: String, name: String, plots: List[(Int, RangePlot)]): Unit =
    for (idxPlot <- plots) {
      val (idx, plot) = idxPlot
      writePlot(path, name, s"$idx", plot)
    }

  @Deprecated
  def writePlotsForDetailsSubname(path: String, name: String, subname: String, plots: List[(Int, RangePlot)]): Unit =
    for (idxPlot <- plots) {
      val (idx, plot) = idxPlot
      writePlot(path, name, s"$subname-$idx", plot)
    }

}
