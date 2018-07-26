package flip.conf.pdf

import flip.conf.cmap.CmapConf
import flip.conf.counter.CounterConf
import flip.pdf.DataBinningDist

trait DataBinningDistConfB[+D <: DataBinningDist[_]] extends SamplingDistConfB[D] {
  val cmap: CmapConf
  val counter: CounterConf
}
