package flip.pdf.monad

import flip.conf.{DistConf, DistConfB}
import flip.measure.Measure
import flip.pdf.Dist

import scala.language.higherKinds

trait DistFunctor[D1[_] <: Dist[_], C <: DistConfB[D1[_]]] {

  def map[A, B](dist: D1[A], f: A => B, measureB: Measure[B], conf: C): D1[B]

}
