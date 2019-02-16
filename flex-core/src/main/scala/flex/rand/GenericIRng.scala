package flex.rand

import cats._
import cats.data._
import cats.implicits._

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

  def next(rng: IRng): (IRng, Out) = nextS.run(rng).value

  def nexts(rng: IRng, n: Int): (List[IRng], List[Out]) =
    (0 until n)
      .foldRight((rng :: Nil, List.empty[Out])) {
        case (_, (rngs, rnds)) => next(rngs.head).bimap(rng1 => rng1 :: rngs, rnd1 => rnd1 :: rnds)
      }
      .bimap(rngs => rngs.reverse.tail, rnds => rnds.reverse)

  def nextInt(rng: IRng, b: Int): (IRng, Int) = next(rng).map(out => floor(out.toFloat * b))

  private def floor(a: Float): Int = if (a - math.round(a) > 0) math.round(a) else math.round(a) - 1

}

trait IRngSyntax {

  type Seed = Long

  type Out = Double

  type IRng = GenericIRng[Seed, Out]

  implicit class IRngSyntaxImpl(rng: IRng) {
    def next: (IRng, Out) = IRng.next(rng)
    def nexts(n: Int): (List[IRng], List[Out]) = IRng.nexts(rng, n)
    def nextInt(b: Int): (IRng, Int) = IRng.nextInt(rng, b)
  }

}

object IRng extends IRngOps {

  private case class IRngImpl(seed: Seed) extends IRng

  def apply(seed: Int): IRng = bare(seed.toLong)

  def bare(seed: Seed): IRng = IRngImpl(seed)

}
