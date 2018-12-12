package flex.conf.pdf

import flex.conf.cmap.CmapConf
import flex.conf.counter.CounterConf
import flex.pdf.DataBinningDist

trait DataBinningDistConfB[+D <: DataBinningDist[_]] extends SamplingDistConfB[D] {
  val cmap: CmapConf
  val counter: CounterConf
}
