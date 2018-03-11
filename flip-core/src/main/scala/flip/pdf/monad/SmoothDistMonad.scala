package flip.pdf.monad

import flip.conf.{SmoothDistConf, SmoothDistConfB}
import flip.pdf.{Dist, SmoothDist}

import scala.language.higherKinds

trait SmoothDistMonad[D1[_] <: SmoothDist[_], D2[_] <: Dist[_], D3[_] <: SmoothDist[_], C <: SmoothDistConfB[D3[_]]]
    extends DistMonad[D1, D2, D3, C] {}
