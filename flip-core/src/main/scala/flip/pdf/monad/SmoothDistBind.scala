package flip.pdf.monad

import flip.conf.SmoothDistConf
import flip.pdf.{Dist, SmoothDist}

import scala.language.higherKinds

trait SmoothDistBind[D1[_]<:SmoothDist[_], D2[_]<:Dist[_], D3[_]<:Dist[_], C<:SmoothDistConf]
  extends DistBind[D1, D2, D3, C] {

}
