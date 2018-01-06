package flip.conf

import flip.pdf.Prim

trait CmapConf {
  val size: Int
  val no: Int
}

object CmapConf {

  def uniform(size: Int, no: Int, start: Option[Prim], end: Option[Prim]): UniformCmapConf =
    UniformCmapConf(size, no, start, end)

  def uniform(size: Int, no: Int, start: Prim, end: Prim): UniformCmapConf =
    UniformCmapConf(size, no, Some(start), Some(end))

}