package flip.pdf.monad

import flip.conf.DistConf
import flip.measure.Measure
import flip.pdf.Dist

import scala.language.higherKinds

trait DistFunctor[D1[_]<:Dist[_]] {

  def map[A, B](dist: D1[A], f: A => B, measureB: Measure[B]): D1[B]

}
