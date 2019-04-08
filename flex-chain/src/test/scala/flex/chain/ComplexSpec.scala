package flex.chain

import flex.chain.Complex.syntax._
import flex.nns.syntax._
import flex.pdf.VQH.syntax._
import flex.rand.IRng
import flex.vec.{SumVec, Vec}
import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalactic.Tolerance._
import org.scalactic.TripleEquals._
import org.scalactic.StringNormalizations._
import org.scalactic.Explicitly._

class ComplexSpec extends Specification with ScalaCheck {

  "Complex" should {

    "construct" in {

      "empty" in {
        val (kin, kout) = (10, 20)
        val complex = Complex.empty(kin, kout)

        val cond1a = complex.in.k == kin
        val cond1b = complex.in.dims.isEmpty
        val cond2 = complex.pools.isEmpty
        val cond3a = complex.out.k == kout
        val cond3b = complex.out.dims.isEmpty
        val cond4 = complex.t.isEmpty

        if (!(cond1a && cond1b)) ko(s"complex.in: ${complex.in}, expected k: $kin")
        else if (!cond2) ko(s"complex.pools: ${complex.pools}, expected: empty")
        else if (!(cond3a && cond3b)) ko(s"complex.out: ${complex.out}, expected k: $kout")
        else if (!cond4) ko(s"complex.t: ${complex.t}, expected: empty")
        else ok
      }

    }

    "ops" in {

      "add" in {
        val (kin, kout) = (10, 20)
        val dimKs = (1 -> 15) :: (2 -> 10) :: (3 -> 5) :: Nil
        val (dims, ks) = dimKs.unzip
        val complex0 = Complex.empty(kin, kout)
        val complex1 = complex0.addDimStd(dimKs)

        val cond1a = complex1.in.dims == dims
        val cond1b = complex1.in.parnns.dims == dims
        val cond2a = complex1.pools.size == dims.size
        val cond2b = complex1.pools.zip(ks).forall { case (vqh, k) => vqh.k == k }
        val cond3a = complex1.out.dims == dims
        val cond3b = complex1.out.parnns.dims == dims

        if (!(cond1a && cond1b)) ko(s"complex.in: ${complex1.in}, expected dim: $dims")
        else if (!(cond2a && cond2b)) ko(s"complex.pools: ${complex1.pools}")
        else if (!(cond3a && cond3b)) ko(s"complex.out: ${complex1.out}, expected dim: $dims")
        else ok
      }

      "map" in {
        val (kin, kout) = (10, 20)
        val dims = 1 :: 2 :: 3 :: Nil
        val ks = Stream.continually(15)
        val complex0 = Complex.empty(kin, kout)
        val complex1 = complex0.addDimStd(dims.zip(ks))
        val complex2 = complex1.map { case _ :: tail => tail }

        val cond1 = complex2.in.dims == dims
        val cond2 = complex2.out.dims == dims.tail

        if (!cond1) ko(s"complex.in: ${complex2.in}, expected dim: $dims")
        else if (!cond2) ko(s"complex.out: ${complex2.out}, expected dim: ${dims.tail}")
        else ok
      }

      "update" in {
        val (kin, kout) = (10, 20)
        val dims = 1 :: 2 :: 3 :: Nil
        val ks = Stream.continually(15)
        val (xs, _) = Vec.std(dims.head, IRng(0))
        val complex0 = Complex.empty(kin, kout)
        val complex1 = complex0.addDimStd(dims.zip(ks))
        val complex2 = complex1.map { case _ :: tail => tail }
        val complex3 = complex2.update(xs)

        val cond1 = complex3.in.ntot === 1.0f +- 0.1f
        val cond2 = complex3.pools.head.ntot === 1.0f +- 0.1f
        val cond3 = complex3.out.ntot === 1.0f +- 0.01f
        val cond4 = complex3.t.size == 1

        if (!cond1) ko(s"complex.in: ${complex3.in}, expected ntot: 1.0")
        else if (!cond2) ko(s"complex.pools.head: ${complex3.pools.head}, expected ntot: 1.0")
        else if (!cond3) ko(s"complex.out: ${complex3.out}, expected ntot: 1.0")
        else if (!cond4) ko(s"complex.t: ${complex3.t}")
        else ok
      }

      "initNormal" in {
        val (kin, kout, kpool) = (10, 20, 15)
        val (dims, ks) = (10 :: 20 :: 30 :: Nil, Stream.continually(kpool))
        val (locs, scales) = (SumVec.zeros(dims), SumVec.ones(dims))
        val complex = Complex.empty(kin, kout).addDimStd(dims.zip(ks)).initNormal(locs, scales)

        val poolSizes = complex.pools.map(pool => pool.size)
        val cond1 = poolSizes.forall(size => size == kpool)

        if (!cond1) ko(s"Size of pools: $poolSizes")
        else ok
      }

      "init" in {
        val (kin, kout, kpool) = (10, 20, 15)
        val (dims, ks) = (10 :: 20 :: 30 :: Nil, Stream.continually(kpool))
        val complex = Complex.empty(kin, kout).addDimStd(dims.zip(ks)).init

        val poolSizes = complex.pools.map(pool => pool.size)
        val cond1 = poolSizes.forall(size => size == kpool)

        if (!cond1) ko(s"Size of pools: $poolSizes")
        else ok
      }

    }

  }

}
