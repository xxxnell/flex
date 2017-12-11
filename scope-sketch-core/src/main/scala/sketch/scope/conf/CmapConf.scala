package sketch.scope.conf

import sketch.scope.pdf.Prim

/**
  * Licensed by Probe Technology, Inc.
  */
trait CmapConf {
  val size: Int
  val no: Int
}

object CmapConf {

  def uniform(size: Int, no: Int, start: Prim, end: Prim): UniformCmapConf =
    UniformCmapConf(size, no, start, end)

}