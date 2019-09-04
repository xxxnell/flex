package flex.measure

import flex.pdf.Prim
import scala.reflect.runtime.universe.{ typeOf, TypeTag }

/**
 * Measure is a bijective map between primitive type of Sketch and arbitrary
 * type.
 * */
trait Measure[A] {

  def apply(a: A): Prim = to(a)

  def to(a: A): Prim

  def from(p: Prim): A

  override def toString: String = s"Measure"

}

trait MeasureSyntax {}

object Measure {

  def apply[A](f: A => Prim, g: Prim => A): Measure[A] = new Measure[A] {
    def to(a: A): Prim = f(a)
    def from(b: Prim): A = g(b)
  }

}
