package sketch.scope.pdf.syntax

import sketch.scope.measure.Measure
import sketch.scope.pdf.{Count, Dist, Prim, Range, Sketch}
import sketch.scope.pdf.monad.{DistFunctor, SketchMonad}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchSyntax extends SketchPropSyntax with SketchMonadSyntax

trait SketchPropSyntax {

  implicit class SketchPropSyntaxImpl[A](sketch: Sketch[A]) {
    def sample: (Sketch[A], A) = Sketch.sample(sketch)
    def update(a: A): Option[Sketch[A]] = Sketch.update(sketch, (a, 1d) :: Nil)
    def update(as: List[(A, Count)]): Option[Sketch[A]] = Sketch.update(sketch, as)
    def count(from: A, to: A): Option[Double] = Sketch.count(sketch, from, to)
    def sum: Double = Sketch.sum(sketch)
    //    def clear: Sketch = Sketch.clear(sketch)
    def probability(from: A, to: A): Option[Double] = Sketch.probability(sketch, from, to)
    def rearrange: Option[Sketch[A]] = Sketch.rearrange(sketch)
    def caDepth: Int = Sketch.caDepth(sketch)
    def caSize: Int = Sketch.caSize(sketch)
    def coDepth: Int = Sketch.coDepth(sketch)
    def coSize: Int = Sketch.coSize(sketch)
  }

}

trait SketchMonadSyntax {

  lazy val sketchMonad: SketchMonad[Sketch, Dist, Sketch] = SketchMonad.pointToPoint

  implicit class SketchMonadSyntaxImpl[A](sketch: Sketch[A]) {
    def map[B](f: A => B)(implicit measureB: Measure[B]): Sketch[B] =
      sketchMonad.map(sketch, f, measureB)
    def flatMap[B, S1<:Sketch[_], S2<:Sketch[_]](f: A => Dist[B])(implicit measureB: Measure[B]): Sketch[B] =
      sketchMonad.bind(sketch, f, measureB)
  }

}