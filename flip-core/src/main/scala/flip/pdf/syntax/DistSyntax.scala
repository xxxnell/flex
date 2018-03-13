package flip.pdf.syntax

import flip.conf._
import flip.measure.Measure
import flip.pdf.arithmetic.CombinationDist
import flip.pdf.monad.{DistBind, DistFunctor}
import flip.pdf.{Dist, PlottedDist, SamplingDist, Sketch}
import flip.plot.{AsciiArtPlot, DensityPlot}
import flip.range.RangeM

import scala.language.higherKinds

trait DistSyntax extends DistPropSyntax with DistMonadSyntax with DistArthmeticSyntax

// props

trait DistPropSyntax {

  implicit class DistPropSyntaxImpl[A](dist: Dist[A]) {
    def probability(from: A, to: A): Double = Dist.probability(dist, from, to)
    def pdf(a: A): Double = Dist.pdf(dist, a)
    def cdf(a: A): Double = Dist.cdf(dist, a)
    def sample: (Dist[A], A) = Dist.sample(dist)
    def samples(n: Int): (Dist[A], List[A]) = Dist.samples(dist, n)
    def sampling: DensityPlot = Dist.sampling(dist)
//    def sampling(ranges: List[RangeM[A]]): PlottedDist[A] =
//      Dist.samplingDist(dist, ranges)
//    def sampling(pltDist: PlottedDist[A]): PlottedDist[A] =
//      Dist.samplingDistForPlottedDist(dist, pltDist)
//    def sampling(smplDist: SamplingDist[A]): PlottedDist[A] =
//      Dist.samplingDistForSamplingDist(dist, smplDist)
//    def uniformSampling(start: A, end: A, size: Int): PlottedDist[A] =
//      Dist.uniformSampling(dist, start, end, size)
//    def histogram(ranges: List[RangeM[A]]): String = AsciiArtPlot.histogram(dist, ranges)
  }

}

// monad

trait DistBindAux[InD[_] <: Dist[_], OutD[_] <: Dist[_]] {
  type Out[A] = OutD[A]
}

trait DistFunctorAux[InD[_] <: Dist[_], OutD[_] <: Dist[_]] {
  type Out[A] = OutD[A]
}

trait DistMonadSyntax extends DistMonadSyntax1 {

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

trait DistMonadSyntax1 extends DistMonadSyntax2 {

  implicit def bindAux1: DistBindAux[Sketch, Sketch] = new DistBindAux[Sketch, Sketch] {}
  implicit def distBind1: DistBind[Dist, Sketch, Sketch, SketchConf] = DistBind.sketch
  implicit def distFunctor1: DistFunctor[Dist, Sketch, SketchConf] = DistFunctor.sketch

}

trait DistMonadSyntax2 extends DistMonadSyntax3 {

  implicit def bindAux2: DistBindAux[SamplingDist, SamplingDist] = new DistBindAux[SamplingDist, SamplingDist] {}
  implicit def distBind2: DistBind[Dist, SamplingDist, SamplingDist, SamplingDistConf] = DistBind.samplingDist
  implicit def distFunctor2: DistFunctor[Dist, SamplingDist, SamplingDistConf] = DistFunctor.samplingDist

}

trait DistMonadSyntax3 {

  implicit def bindAux3: DistBindAux[Dist, SamplingDist] = new DistBindAux[Dist, SamplingDist] {}
  implicit def distBind3: DistBind[Dist, Dist, SamplingDist, SamplingDistConf] = DistBind.dist

}

// arthemetic

trait DistArthmeticSyntax {

  implicit class DistArthmeticSyntaxImpl1[A](weightDist: (Double, Dist[A])) {
    def +(weightDist2: (Double, Dist[A]))(implicit measure: Measure[A], conf: DistConf): CombinationDist[A] =
      CombinationDist.apply(weightDist2, weightDist)
  }

  implicit class DistArthmeticSyntaxImpl2[A](combi: CombinationDist[A]) {
    def +(weightDist2: (Double, Dist[A]))(implicit measure: Measure[A], conf: DistConf): CombinationDist[A] =
      CombinationDist.apply((weightDist2 :: combi.components).toList: _*)
  }

}
