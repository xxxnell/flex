package flip.chain.monad

import flip.conf.pdf.SamplingDistConfB
import flip.pdf.{Dist, SamplingDist, SmoothDist}

import scala.language.higherKinds

trait SmoothDistBind[D1[_] <: SmoothDist[_], D2[_] <: Dist[_], D3[_] <: SamplingDist[_], C <: SamplingDistConfB[D3[_]]]
    extends DistBind[D1, D2, D3, C] {}
