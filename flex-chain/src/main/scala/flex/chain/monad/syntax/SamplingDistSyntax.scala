package flex.chain.monad.syntax

import flex.chain.monad.{SamplingDistBind, SamplingDistFunctor}
import flex.conf.pdf.{SamplingDistConf, SamplingDistConfB, SketchConf}
import flex.measure.Measure
import flex.pdf.{Dist, SamplingDist, Sketch}

import scala.language.higherKinds

trait SamplingDistSyntax extends SamplingDistSyntax1 {

  implicit class SamplingDistMonadSyntaxImpl[A](dist: SamplingDist[A]) {
    def map[B, D[_] <: SamplingDist[_], C <: SamplingDistConfB[D[_]]](
        f: A => B)(implicit functor: SamplingDistFunctor[SamplingDist, D, C], measureB: Measure[B], conf: C): D[B] =
      functor.map(dist, f, measureB, conf)
    def flatMap[B, D1[_] <: Dist[_], D2[_] <: SamplingDist[_], C <: SamplingDistConfB[D2[_]]](
        f: A => D1[B])(implicit bind: SamplingDistBind[SamplingDist, D1, D2, C], measureB: Measure[B], conf: C): D2[B] =
      bind.bind(dist, f, measureB, conf)
  }

}

trait SamplingDistSyntax1 extends SamplingDistSyntax2 {

  implicit def samplingDistBind1: SamplingDistBind[SamplingDist, Sketch, Sketch, SketchConf] =
    SamplingDistBind.sketch
  implicit def samplingDistFunctor: SamplingDistFunctor[SamplingDist, SamplingDist, SamplingDistConf] =
    SamplingDistFunctor.default

}

trait SamplingDistSyntax2 extends SamplingDistSyntax3 {

  implicit def samplingDistBind2: SamplingDistBind[SamplingDist, SamplingDist, SamplingDist, SamplingDistConf] =
    SamplingDistBind.samplingDist

}

trait SamplingDistSyntax3 {

  implicit def samplingDistBind3: SamplingDistBind[SamplingDist, Dist, SamplingDist, SamplingDistConf] =
    SamplingDistBind.dist

}
