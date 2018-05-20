package flip.pdf.syntax

import flip.conf.pdf.{SamplingDistConf, SamplingDistConfB, SketchConf}
import flip.measure.Measure
import flip.pdf.monad.{SamplingDistBind, SamplingDistFunctor}
import flip.pdf.{Dist, SamplingDist, Sketch}
import flip.plot.PointPlot

import scala.language.higherKinds

trait SamplingDistSyntax extends SamplingDistPropSyntax with SamplingDistMonadSyntax

trait SamplingDistPropSyntax {

  implicit class SamplingDistPropSyntaxImpl[A](dist: SamplingDist[A]) {
    def sample: (SamplingDist[A], A) = SamplingDist.sample(dist)
    def pdf(a: A): Double = SamplingDist.pdf(dist, a)
    def samples(n: Int): (SamplingDist[A], List[A]) = SamplingDist.samples(dist, n)
    def sampling: PointPlot = SamplingDist.sampling(dist)
    def densityPlot: PointPlot = SamplingDist.sampling(dist)
    def pdfPlot: PointPlot = SamplingDist.sampling(dist)
  }

}

trait SamplingDistMonadSyntax extends SamplingDistMonadSyntax1 {

  implicit class SamplingDistMonadSyntaxImpl[A](dist: SamplingDist[A]) {
    def map[B, D[_] <: SamplingDist[_], C <: SamplingDistConfB[D[_]]](
        f: A => B)(implicit functor: SamplingDistFunctor[SamplingDist, D, C], measureB: Measure[B], conf: C): D[B] =
      functor.map(dist, f, measureB, conf)
    def flatMap[B, D1[_] <: Dist[_], D2[_] <: SamplingDist[_], C <: SamplingDistConfB[D2[_]]](
        f: A => D1[B])(implicit bind: SamplingDistBind[SamplingDist, D1, D2, C], measureB: Measure[B], conf: C): D2[B] =
      bind.bind(dist, f, measureB, conf)
  }

}

trait SamplingDistMonadSyntax1 extends SamplingDistMonadSyntax2 {

  implicit def samplingDistBind1: SamplingDistBind[SamplingDist, Sketch, Sketch, SketchConf] =
    SamplingDistBind.sketch
  implicit def samplingDistFunctor: SamplingDistFunctor[SamplingDist, SamplingDist, SamplingDistConf] =
    SamplingDistFunctor.default

}

trait SamplingDistMonadSyntax2 extends SamplingDistMonadSyntax3 {

  implicit def samplingDistBind2: SamplingDistBind[SamplingDist, SamplingDist, SamplingDist, SamplingDistConf] =
    SamplingDistBind.samplingDist

}

trait SamplingDistMonadSyntax3 {

  implicit def samplingDistBind3: SamplingDistBind[SamplingDist, Dist, SamplingDist, SamplingDistConf] =
    SamplingDistBind.dist

}
