package flex.chain.monad.syntax

import flex.chain.monad.{SketchBind, SketchFunctor}
import flex.conf.pdf.SketchConf
import flex.measure.Measure
import flex.pdf.{Dist, Sketch}

trait SketchSyntax extends SketchSyntax1 {

  implicit class SketchMonadSyntaxImpl[A](sketch: Sketch[A]) {
    def map[B](f: A => B)(implicit functor: SketchFunctor[Sketch, Sketch, SketchConf],
                          measureB: Measure[B],
                          conf: SketchConf): Sketch[B] =
      functor.map(sketch, f, measureB, conf)
    def flatMap[B](f: A => Dist[B])(implicit bind: SketchBind[Sketch, Dist, Sketch, SketchConf],
                                    measureB: Measure[B],
                                    conf: SketchConf): Sketch[B] =
      bind.bind(sketch, f, measureB, conf)
  }

}

trait SketchSyntax1 {

  implicit def sketchFunctor: SketchFunctor[Sketch, Sketch, SketchConf] = SketchFunctor.default
  implicit def sketchBind: SketchBind[Sketch, Dist, Sketch, SketchConf] = SketchBind.default

}
