package flip.pdf.monad

import flip.conf.pdf.SmoothDistConf
import flip.measure.Measure
import flip.pdf.{DeltaDist, Dist}

object Pure {

  def apply[A](a: A, measure: Measure[A], conf: SmoothDistConf): Dist[A] = pure(a, measure, conf)

  def pure[A](a: A, measure: Measure[A], conf: SmoothDistConf): Dist[A] = {
    val conf = SmoothDistConf.default
    DeltaDist(measure, conf, a)
  }

}
