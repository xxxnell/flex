package sketch.scope.rand

import cats.data.State

import scala.util.Random

/**
  * Licensed by Probe Technology, Inc.
  */
trait GenericIRng[S<:Random, A<:Double] extends Rng[S, A]

trait IRngOps extends RngOps[Random, Double, GenericIRng] {

  // todo note that it is not functional now
  def nextS: State[IRng, Double] = State { rng =>
    val random = rng.seed
    (IRng.bare(random), random.nextDouble)
  }

}

trait IRngSyntax {

  type IRng = GenericIRng[Random, Double]

  implicit class IRngSyntaxImpl(rng: IRng) {
    def next: (IRng, Double) = IRng.nextS.run(rng).value
  }

}

object IRng extends IRngOps {

  case class IRngImpl(seed: Random) extends IRng

  def apply(seed: Int): IRng = bare(new Random(seed))

  def apply(seed: Long): IRng = bare(new Random(seed))

  def bare(seed: Random): IRng = IRngImpl(seed)

}
