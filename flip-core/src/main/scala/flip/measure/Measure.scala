package flip.measure

import flip.pdf.Prim
import scala.reflect.runtime.universe.{typeOf, TypeTag}

/**
  * Measure of A provides bijective map between A and Prim.
  * */
trait Measure[A] {

  def apply(a: A): Prim = to(a)

  def to(a: A): Prim

  def from(p: Prim): A

  override def toString: String = s"Measure"

}

trait MeasureSyntax {

}

object Measure {

  def apply[A](f: A => Prim, g: Prim => A): Measure[A] = new Measure[A] {
    def to(a: A): Prim = f(a)
    def from(b: Prim): A = g(b)
  }

}