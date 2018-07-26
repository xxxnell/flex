package flip.rand

import cats.data.State

import scala.util.Random
import scala.util.hashing.byteswap64

trait GenericIRng[S <: Seed, A <: Out] extends Rng[S, A]

trait IRngOps extends RngOps[Seed, Out, GenericIRng] {

  def nextS: State[IRng, Out] = State { rng =>
    val seed1 = byteswap64(rng.seed)
    val seed2 = new Random(seed1).nextLong()
    val rand = new Random(seed1).nextDouble()

    (IRng.bare(seed2), rand)
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
