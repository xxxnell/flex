package flex.chain.monad

import flex.conf.pdf.{SketchConf, SketchConfB}
import flex.pdf.Sketch

import scala.language.higherKinds

trait SketchFunctor[D1[_] <: Sketch[_], D2[_] <: Sketch[_], C <: SketchConfB[D2[_]]]
    extends SamplingDistFunctor[D1, D2, C]

object SketchFunctor {

  def default: SketchFunctor[Sketch, Sketch, SketchConf] = CombinatorialSketchFunctor

}
