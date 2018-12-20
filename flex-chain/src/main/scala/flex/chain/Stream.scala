package flex.chain

import cats.Eval
import flex.pdf.Dist

trait Stream[A] extends Model {

  private[chain] val op: Eval[Map[Int, Dist[A]]]

}

trait StreamOps extends ModelOps {

  def map[A](model: Stream[A], f: Map[Int, Double] => Map[Int, Double]): Stream[A] = ???

  def flatMap[A](model: Stream[A], f: Map[Int, Double] => Stream[A]): Stream[A] = ???

  def update[A](s: Stream[A], xobs: Map[Int, Double]): Model

  def input[A](mode: Model): Map[Int, Dist[A]]

  def output[A](model: Model): Map[Int, Dist[A]]

  def contract(model: Model): Model

}

trait StreamSyntax {

  implicit class StreamSyntaxImpl[A](model: Stream[A]) {
    def map(f: Map[Int, Double] => Map[Int, Double]): Stream[A] = Stream.map(model, f)
    def flatMap(f: Map[Int, Double] => Stream[A]): Stream[A] = Stream.flatMap(model, f)
    def input: Map[Int, Dist[A]] = Stream.input(model)
    def output: Map[Int, Dist[A]] = Stream.output(model)
    def contract: Stream[A] = Stream.contract(model)
  }

}

object Stream extends StreamOps {

  private case class StreamImpl[A](op: Eval[Map[Int, Dist[A]]]) extends Stream[A]

  def apply[A](xs: Map[Int, Dist[A]]): Stream[A] = StreamImpl(Eval.now(xs))

  object syntax extends StreamSyntax

}