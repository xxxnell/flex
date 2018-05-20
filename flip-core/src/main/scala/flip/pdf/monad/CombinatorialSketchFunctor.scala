package flip.pdf.monad

import flip.conf.pdf.{SketchConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.Sketch

object CombinatorialSketchFunctor extends SketchFunctor[Sketch, Sketch, SketchConf] {

  def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B], conf: SketchConf): Sketch[B] =
    SketchBind.default.bind(dist, (a: A) => Pure(f(a), measureB, SmoothDistConf.forDistConf(conf)), measureB, conf)

}
