package sketch.scope.pdf.monad

import sketch.scope.conf.SmoothDistConf
import sketch.scope.pdf.{Dist, SmoothDist}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistMonad[D1[_]<:SmoothDist[_], D2[_]<:Dist[_], D3[_]<:Dist[_], C<:SmoothDistConf]
  extends DistMonad[D1, D2, D3, C] {

}
