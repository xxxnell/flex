package flex.chain

import flex.pdf.Dist

trait Model {}

trait ModelOps {}

trait ModelSyntax {

  implicit class ModelSyntaxImpl(model: Model) {}

}

object Model extends ModelOps {

  def stream[A](xs: Map[Int, Dist[A]]): Stream[A] = Stream(xs)

  def stream[A](xs: Dist[A]*): Stream[A] = ???

  object syntax extends ModelSyntax

}