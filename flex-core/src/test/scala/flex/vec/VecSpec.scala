package flex.vec

import flex.rand.IRng
import org.nd4j.linalg.factory.Nd4j
import org.specs2.mutable._
import org.specs2.ScalaCheck

class VecSpec extends Specification with ScalaCheck {

  "Vec" should {

    "construct" in {

      "apply" in {
        val as = List.range(0, 784)
        val v = Vec(as)

        val cond1 = v.dim == as.size

        if (!cond1) ko(s"dim(v): ${v.dim}, expected: ${as.size}")
        else ok
      }

      "int" in {
        val as = List.range(0, 784)
        val v = Vec.int(as)

        val cond1 = v.dim == as.size

        if (!cond1) ko(s"dim(v): ${v.dim}, expected: ${as.size}")
        else ok
      }

      "std" in {
        val (dim, seed) = (10, 0)
        val (v, rng1) = Vec.std(dim, IRng(seed))

        val cond1 = v.dim == dim
        val cond2 = rng1.seed != seed

        if (!cond1) ko(s"dim(v): ${v.dim}, expected: $dim")
        else if (!cond2) ko(s"seed doesn't change: $seed -> ${rng1.seed}")
        else ok
      }

      "stds" in {
        val (dims, seed) = (10 :: 20 :: 30 :: Nil, 0)
        val (vs, rng1) = Vec.stds(dims, IRng(seed))

        val cond1 = vs.zip(dims).forall { case (v, dim) => v.dim == dim }
        val cond2 = rng1.seed != seed

        if (!cond1) ko(s"dim(v): ${vs.map(v => v.dim)}, expected: $dims")
        else if (!cond2) ko(s"seed doesn't change: $seed -> ${rng1.seed}")
        else ok
      }

      "zeros" in {
        val dim = 10
        val v = Vec.zeros(dim)

        val cond1 = v.dim == dim

        if (!cond1) ko(s"dim(v): ${v.dim}, expected: $dim")
        else ok
      }

    }

    "ops" in {

      "dim" in {
        val as = 1.0 :: 2.0 :: 3.0 :: Nil
        val v = Vec(as)

        val cond1 = v.dim == as.size

        if (!cond1) ko(s"v: $v, expected: ${as.size}")
        else ok
      }

    }

  }

}
