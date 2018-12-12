package flex.pdf.arithmetic

import flex.conf.pdf.DistConf
import flex.measure.Measure
import flex.pdf.Dist

object Sum {

  def weightedSum[A](weightDists: List[(Double, Dist[A])], measureB: Measure[A], conf: DistConf): CombinationDist[A] = {
    CombinationDist(weightDists: _*)(measureB, conf)
  }

}
