package sketch.scope.pdf.syntax

import sketch.scope.pdf.{Dist, Prim, Range, Sketch}
import sketch.scope.pdf.monad.{DistFunctor, SketchMonad}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchSyntax extends SketchPropSyntax with SketchMonadSyntax

trait SketchPropSyntax {

  implicit class SketchPropSyntaxImpl[A](sketch: Sketch[A]) {
    def update(a: A): Option[Sketch[A]] = ??? // Sketch.update(sketch, a)
    def count(from: A, to: A): Option[Double] = Sketch.count(sketch, from, to)
    def sum: Double = Sketch.sum(sketch)
    //    def clear: Sketch = Sketch.clear(sketch)
    def probability(from: A, to: A): Option[Double] = Sketch.probability(sketch, from, to)
    def rearrange: Option[Sketch[A]] = Sketch.rearrange(sketch)
  }

}

trait SketchMonadSyntax {

  implicit class SketchMonadSyntaxImpl[A](sketch: Sketch[A]) {
    def map[B](f: A => B): Sketch[B] = SketchMonad.pointToPoint.map(sketch, f)
    def flatMap[B, S1<:Sketch[_], S2<:Sketch[_]](f: A => Dist[B]): Sketch[B] = SketchMonad.pointToPoint.bind(sketch, f)
  }

}