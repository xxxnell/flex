package flex.conf.cmap

import flex.pdf.Prim

trait UniformCmapConf extends InitialCmapConf {

  def start: Option[Prim]

  def end: Option[Prim]

}
