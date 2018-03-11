package flip.pdf.monad

import flip.conf.SketchConf
import flip.measure.Measure
import flip.pdf.{Dist, Sketch}

object PointToPointSketchMonad extends SketchMonad[Sketch, Dist, Sketch, SketchConf] {

  def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B], conf: SketchConf): Sketch[B] =
    bind(dist, (a: A) => pure(f(a), measureB), measureB, conf)

  def bind[A, B](dist: Sketch[A], f: A => Dist[B], measureB: Measure[B], conf: SketchConf): Sketch[B] =
    PointToPointSketchBind.bind(dist, f, measureB, conf)

}
