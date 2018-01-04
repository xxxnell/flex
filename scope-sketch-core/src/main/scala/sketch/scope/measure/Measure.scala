package sketch.scope.measure

import sketch.scope.pdf.Prim

/**
  * Measure of A provides bijective map between A and Prim.
  * */
trait Measure[A] {

  def apply(a: A): Prim = to(a)

  def to(a: A): Prim

  def from(p: Prim): A

}

trait MeasureSyntax {

}

object Measure {

  def apply[A](f: A => Prim, g: Prim => A): Measure[A] = new Measure[A] {
    def to(a: A): Prim = f(a)
    def from(b: Prim): A = g(b)
  }

}