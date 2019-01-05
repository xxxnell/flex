package flex.pdf

import flex.rand._

trait Bernoulli {

  val p: Float

  val rng: IRng

}

trait BernoulliOps {

  def sample(b: Bernoulli): (Bernoulli, Int) = {
    val (rng, rand) = b.rng.next
    (Bernoulli(b.p, rng), if (b.p >= rand) 1 else 0)
  }

}

trait BernoulliSyntax {

  implicit class BernoulliSyntaxImpl(bernoulli: Bernoulli) {
    def sample: (Bernoulli, Int) = Bernoulli.sample(bernoulli)
  }

}

object Bernoulli extends BernoulliOps {

  private case class BernoulliImpl(p: Float, rng: IRng) extends Bernoulli

  object syntax extends BernoulliSyntax

  def apply(p: Float, rng: IRng): Bernoulli = BernoulliImpl(p, rng)

  def apply(p: Float): Bernoulli = apply(p, IRng(p.hashCode))

  def apply(p: Double): Bernoulli = apply(p.toFloat)

}
