package flex.nns

import flex.rand.IRng
import flex.vec.Vec
import org.specs2.mutable._
import org.specs2.ScalaCheck

class VecLSHSpec extends Specification with ScalaCheck {

  "LSH" should {

    "hash" in {
      val (dim, l, memoSize, rng0) = (1, 3, 1, IRng(0))
      val (lsh, rng1) = VecLSH.apply(dim, List.fill(l)(1.0f), memoSize, rng0)
      val (x, _) = Vec.std(dim, rng1)
      val hash = lsh.hash(x)

      val cond1 = hash.size == l

      if (!cond1) ko(s"hash: $hash, expected size: $l") else ok
    }

    "hash2" in {
      val (dim, l, memoSize, rng0) = (1, 3, 1, IRng(0))
      val (lsh, rng1) = VecLSH.apply(dim, List.fill(l)(1.0f), memoSize, rng0)
      val (x, _) = Vec.std(dim, rng1)
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
