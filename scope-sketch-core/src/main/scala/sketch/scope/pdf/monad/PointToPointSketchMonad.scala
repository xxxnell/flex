package sketch.scope.pdf.monad

import sketch.scope.conf.SketchConf
import sketch.scope.measure.Measure
import sketch.scope.pdf.{Dist, Sketch}

object PointToPointSketchMonad extends SketchMonad[Sketch, Dist, Sketch, SketchConf] {

  def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B], conf: SketchConf): Sketch[B] =
    bind(dist, (a: A) => pure(f(a), measureB), measureB, conf)

  def bind[A, B](dist: Sketch[A], f: A => Dist[B], measureB: Measure[B], conf: SketchConf): Sketch[B] =
    PointToPointSketchBind.bind(dist, f, measureB, conf)

}
