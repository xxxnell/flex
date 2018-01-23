package flip.pdf.monad

import flip.conf.SketchConf
import flip.measure.Measure
import flip.pdf.{Dist, Sketch}

object PointToPointSketchMonad extends SketchMonad[Sketch, Dist, Sketch] {

  def map[A, B](dist: Sketch[A], f: A => B, measureB: Measure[B]): Sketch[B] =
    bind(dist, (a: A) => pure(f(a), measureB), measureB)

  def bind[A, B](dist: Sketch[A], f: A => Dist[B], measureB: Measure[B]): Sketch[B] =
    PointToPointSketchBind.bind(dist, f, measureB)

}
