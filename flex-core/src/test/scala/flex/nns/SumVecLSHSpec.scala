package flex.nns

import flex.rand.IRng
import flex.vec.SumVec
import org.specs2.mutable._
import org.specs2.ScalaCheck

class SumVecLSHSpec extends Specification with ScalaCheck {

  "SumVecLSH" should {

    "hash" in {
      val (dim, depth, l, memoSize, rng0) = (1, 5, 3, 10, IRng(0))
      val dims = List.fill(depth)(dim)
      val (lsh, rng1) = SumVecLSH(dims, List.fill(l)(1.0f), memoSize, rng0)
      val (x, _) = SumVec.std(dims, rng1)
      val hash = lsh.hash(x)

      val cond1 = hash.size == l

      if (!cond1) ko(s"hash: $hash, expected size: $l") else ok
    }

    "hash2" in {
      val (dim, depth, l, memoSize, rng0) = (1, 10, 3, 10, IRng(0))
      val dims = List.fill(depth)(dim)
      val w = List.fill(l)(1.0f)
      val (lsh, rng1) = SumVecLSH(dims, w, memoSize, rng0)
      val (x, _) = SumVec.std(dims, rng1)
      val hash1 = lsh.hash(x)
      val hash2 = lsh.hash(x)

      val cond1 = hash1.size == l
      val cond2 = hash1 == hash2

      if (!cond1) ko(s"hash: $hash1, expected size: $l")
      else if (!cond2) ko(s"hash1: $hash1, hash2: $hash2")
      else ok
    }

  }

}
