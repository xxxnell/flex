package flex.chain.monad.syntax

import flex.chain.monad.{DistBind, DistFunctor}
import flex.conf.pdf.{SamplingDistConf, SamplingDistConfB, SketchConf}
import flex.measure.Measure
import flex.pdf.{Dist, SamplingDist, Sketch}

import scala.language.higherKinds

trait DistSyntax extends DistSyntax1 {

  implicit class DistMonadSyntaxImpl0[A](dist: Dist[A]) {
    def map[B, D[_] <: SamplingDist[_], C <: SamplingDistConfB[D[_]]](f: A => B)(implicit
                                                                                 functor: DistFunctor[Dist, D, C],
                                                                                 measureB: Measure[B],
                                                                                 conf: C): D[B] =
      functor.map(dist, f, measureB, conf)
    def flatMap[B, D1[_] <: Dist[_], D2[_] <: SamplingDist[_], C <: SamplingDistConfB[D2[_]]](f: A => D1[B])(
        implicit
        aux: DistBindAux[D1, D2],
        bind: DistBind[Dist, D1, D2, C],
        measureB: Measure[B],
        conf: C): aux.Out[B] =
      bind.bind(dist, f, measureB, conf)
  }

}

trait DistSyntax1 extends DistSyntax2 {

  implicit def bindAux1: DistBindAux[Sketch, Sketch] = new DistBindAux[Sketch, Sketch] {}
  implicit def distBind1: DistBind[Dist, Sketch, Sketch, SketchConf] = DistBind.sketch
  //  implicit def distFunctor1: DistFunctor[Dist, Sketch, SketchConf] = DistFunctor.sketch

}

trait DistSyntax2 extends DistSyntax3 {

  implicit def bindAux2: DistBindAux[SamplingDist, SamplingDist] = new DistBindAux[SamplingDist, SamplingDist] {}
  implicit def distBind2: DistBind[Dist, SamplingDist, SamplingDist, SamplingDistConf] = DistBind.samplingDist
  implicit def distFunctor2: DistFunctor[Dist, SamplingDist, SamplingDistConf] = DistFunctor.samplingDist

}

trait DistSyntax3 {

  implicit def bindAux3: DistBindAux[Dist, SamplingDist] = new DistBindAux[Dist, SamplingDist] {}
  implicit def distBind3: DistBind[Dist, Dist, SamplingDist, SamplingDistConf] = DistBind.dist

}

// aux

trait DistBindAux[InD[_] <: Dist[_], OutD[_] <: Dist[_]] {
  type Out[A] = OutD[A]
}

trait DistFunctorAux[InD[_] <: Dist[_], OutD[_] <: Dist[_]] {
  type Out[A] = OutD[A]
}
