package flip.conf

import flip.pdf.Prim

trait UniformCmapConf extends CmapConf {
  val start: Option[Prim]
  val end: Option[Prim]
}

object UniformCmapConf {

  case class UniformCmapConfImpl(size: Int, no: Int, start: Option[Prim], end: Option[Prim]) extends UniformCmapConf

  def apply(size: Int, no: Int, start: Option[Prim], end: Option[Prim]): UniformCmapConf =
    UniformCmapConfImpl(size, no, start, end)

}
