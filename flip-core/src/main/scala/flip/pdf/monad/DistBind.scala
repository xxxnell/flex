package flip.pdf.monad

import flip.conf.DistConf
import flip.measure.Measure
import flip.pdf.Dist

import scala.language.higherKinds

trait DistBind[D1[_]<:Dist[_], D2[_]<:Dist[_], D3[_]<:Dist[_], C<:DistConf] {

  def bind[A, B](dist: D1[A], f: A => D2[B], measureB: Measure[B], conf: C): D3[B]

}
