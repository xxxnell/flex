package flip.conf

import flip.pdf.Prim

trait UniformCmapConf extends InitialCmapConf {

  def start: Option[Prim]

  def end: Option[Prim]

}
