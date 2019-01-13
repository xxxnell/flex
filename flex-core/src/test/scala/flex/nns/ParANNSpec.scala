package flex.nns

import flex.rand.IRng
import flex.nns.syntax._
import org.specs2.mutable._
import org.specs2.ScalaCheck

class ParANNSpec extends Specification with ScalaCheck {

  "ParANN" should {

    "construct" in {
      val (l, dims, rng) = (10, 1 :: 2 :: 3 :: Nil, IRng(0))
      val (ann, _) = ParANN.empty(l, dims, rng)

      val cond1 = ann.arrAnns.zip(dims).forall { case (_ann, dim) => _ann.lshs.forall(lsh => lsh.dim == dim) }
      val cond2 = ann.compMap.isEmpty

      if (!cond1) ko(s"arrAnns: ${ann.arrAnns}")
      else if (!cond2) ko(s"compMap: ${ann.compMap}")
      else ok
    }

  }

}
