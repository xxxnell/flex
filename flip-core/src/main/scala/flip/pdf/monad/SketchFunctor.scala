package flip.pdf.monad

import flip.conf.{SketchConf, SketchConfB, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.Sketch

import scala.language.higherKinds

trait SketchFunctor[D[_] <: Sketch[_], C <: SketchConfB[D[_]]] extends SamplingDistFunctor[D, C]

object SketchFunctor {

  object SketchFunctor extends SketchFunctor[Sketch, SketchConf] {

    def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B], conf: SketchConf): Sketch[B] =
      SketchBind().bind(dist, (a: A) => Pure(f(a), measureB, SmoothDistConf.forDistConf(conf)), measureB, conf)

  }

  def apply(): SketchFunctor[Sketch, SketchConf] = SketchFunctor

}
