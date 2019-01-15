package flex.vec

import flex.rand.IRng
import org.specs2.mutable._
import org.specs2.ScalaCheck

class VecSpec extends Specification with ScalaCheck {

  "Vec" should {

    "construct" in {

      "apply" in {
        val as = 1.0 :: 2.0 :: 3.0 :: Nil
        val v = Vec(as)

        val cond1 = v.dim == as.size

        if (!cond1) ko(s"dim(v): ${v.dim}, expected: ${as.size}")
        else ok
      }

      "std" in {
        val dim = 10
        val (v, _) = Vec.std(dim, IRng(0))

        val cond1 = v.dim == dim

        if (!cond1) ko(s"dim(v): ${v.dim}, expected: $dim")
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
