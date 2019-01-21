package flex.chain

import flex.chain.Complex.syntax._
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.vec.Vec
import org.scalactic.Tolerance._
import org.specs2.ScalaCheck
import org.specs2.mutable._

class ComplexSpec extends Specification with ScalaCheck {

  "Complex" should {

    "construct" in {

      "empty" in {
        val (kin, kout) = (10, 20)
        val complex = Complex.empty(kin, kout)

        val cond1a = complex.vqhin.k == kin
        val cond1b = complex.vqhin.dims.isEmpty
        val cond2a = complex.vqhout.k == kout
        val cond2b = complex.vqhout.dims.isEmpty

        if (!(cond1a && cond1b)) ko(s"complex.vqhin: ${complex.vqhin}, expected k: $kin")
        else if (!(cond2a && cond2b)) ko(s"complex.vqhout: ${complex.vqhout}, expected k: $kout")
        else ok
      }

    }

    "ops" in {

      "add" in {
        val (kin, kout) = (10, 20)
        val dims = 1 :: 2 :: 3 :: Nil
        val complex0 = Complex.empty(kin, kout)
        val complex1 = complex0.addStd(dims)

        val cond1 = complex1.vqhin.dims == dims
        val cond2 = complex1.vqhout.dims == dims

        if (!cond1) ko(s"complex.vqhin: ${complex1.vqhin}, expected dim: $dims")
        else if (!cond2) ko(s"complex.vqhout: ${complex1.vqhout}, expected dim: $dims")
        else ok
      }

      "map" in {
        val (kin, kout) = (10, 20)
        val dims = 1 :: 2 :: 3 :: Nil
        val complex0 = Complex.empty(kin, kout)
        val complex1 = complex0.addStd(dims)
        val complex2 = complex1.map { case _ :: tail => tail }

        val cond1 = complex2.vqhin.dims == dims
        val cond2 = complex2.vqhout.dims == dims.tail

        if (!cond1) ko(s"complex.vqhin: ${complex2.vqhin}, expected dim: $dims")
        else if (!cond2) ko(s"complex.vqhout: ${complex2.vqhout}, expected dim: ${dims.tail}")
        else ok
      }

      "update" in {
        val (kin, kout) = (10, 20)
        val dims = 1 :: 2 :: 3 :: Nil
        val (xs, _) = Vec.std(dims.head, IRng(0))
        val complex0 = Complex.empty(kin, kout)
        val complex1 = complex0.addStd(dims)
        val complex2 = complex1.map { case _ :: tail => tail }
        val complex3 = complex2.update(xs)

        val cond1 = complex3.vqhin.ntot === 1.0 +- 0.01
        val cond2 = complex3.vqhout.ntot === 1.0 +- 0.01
        val cond3 = complex3.t.size == 1

        if (!cond1) ko(s"complex.vqhin: ${complex2.vqhin}")
        else if (!cond2) ko(s"complex.vqhout: ${complex2.vqhout}")
        else if (!cond3) ko(s"complex.t: ${complex3.t}")
        else ok
      }

    }

  }

}
