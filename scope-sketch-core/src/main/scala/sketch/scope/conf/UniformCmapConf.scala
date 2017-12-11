package sketch.scope.conf

import sketch.scope.pdf.Prim

/**
  * Licensed by Probe Technology, Inc.
  */
trait UniformCmapConf extends CmapConf {
  val start: Prim
  val end: Prim
}

object UniformCmapConf {

  case class UniformCmapConfImpl(size: Int, no: Int, start: Prim, end: Prim) extends UniformCmapConf

  def apply(size: Int, no: Int, start: Prim, end: Prim): UniformCmapConf =
    UniformCmapConfImpl(size, no, start, end)

}