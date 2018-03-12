package flip.pdf.monad

import flip.conf.{DistConf, DistConfB}
import flip.measure.Measure
import flip.pdf.Dist

import scala.language.higherKinds

trait DistFunctor[D[_] <: Dist[_], C <: DistConfB[D[_]]] {

  def map[A, B](dist: D[A], f: A => B, measureB: Measure[B], conf: C): D[B]

}
