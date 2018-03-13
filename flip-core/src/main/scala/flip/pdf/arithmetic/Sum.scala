package flip.pdf.arithmetic

import flip.conf.DistConf
import flip.measure.Measure
import flip.pdf.Dist

object Sum {

  def weightedSum[A](weightDists: List[(Double, Dist[A])], measureB: Measure[A], conf: DistConf): CombinationDist[A] = {
    CombinationDist(weightDists: _*)(measureB, conf)
  }

}
