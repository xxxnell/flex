package sketch.scope.rand

import cats.data.State

import scala.util.Random

/**
  * Licensed by Probe Technology, Inc.
  */
trait GenericIRng[S<:Seed, A<:Out] extends Rng[S, A]

trait IRngOps extends RngOps[Seed, Out, GenericIRng] {

  def nextS: State[IRng, Out] = State { rng =>
    val seed = new Random(rng.seed).nextLong()
    val rand = new Random(rng.seed).nextDouble()

    (IRng.bare(seed), rand)
  }

}

trait IRngSyntax {

  type Seed = Long

  type Out = Double

  type IRng = GenericIRng[Seed, Out]

  implicit class IRngSyntaxImpl(rng: IRng) {
    def next: (IRng, Out) = IRng.nextS.run(rng).value
  }

}

object IRng extends IRngOps {

  case class IRngImpl(seed: Seed) extends IRng

  def apply(seed: Int): IRng = bare(seed.toLong)

  def bare(seed: Seed): IRng = IRngImpl(seed)

}
