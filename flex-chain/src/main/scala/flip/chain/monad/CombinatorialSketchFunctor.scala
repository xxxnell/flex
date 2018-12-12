package flex.chain.monad

import flex.conf.pdf.{SketchConf, SmoothDistConf}
import flex.measure.Measure
import flex.pdf.Sketch

object CombinatorialSketchFunctor extends SketchFunctor[Sketch, Sketch, SketchConf] {

  def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B], conf: SketchConf): Sketch[B] =
    SketchBind.default.bind(dist, (a: A) => Pure(f(a), measureB, SmoothDistConf.forDistConf(conf)), measureB, conf)

}
