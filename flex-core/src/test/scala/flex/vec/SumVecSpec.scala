package flex.vec

import flex.rand.IRng
import org.specs2.mutable._
import org.specs2.ScalaCheck

class SumVecSpec extends Specification with ScalaCheck {

  "SumVec" should {

    "construct" in {

      "std" in {
        val dims = 1 :: 2 :: 3 :: Nil
        val (v, _) = SumVec.std(dims, IRng(0))

        val cond1 = v.dim == dims.sum

        if (!cond1) ko(s"dim(v): v.dim, expected: ${dims.sum}")
        else ok
      }

      "zeros" in {
        val dims = 1 :: 2 :: 3 :: Nil
        val v = SumVec.zeros(dims)

        val cond1 = v.dim == dims.sum

        if (!cond1) ko(s"dim(v): v.dim, expected: ${dims.sum}")
        else ok
      }

    }

    "ops" in todo

  }

}
