package flex.nns

import flex.rand.IRng
import flex.nns.syntax._
import org.specs2.mutable._
import org.specs2.ScalaCheck

class ParVecANNSpec extends Specification with ScalaCheck {

  "ParVecANN" should {

    "construct" in {
      val (l, dims, rng) = (10, 1 :: 2 :: 3 :: Nil, IRng(0))
      val (ann, _) = ParVecANN.empty(l, dims, rng)

      val cond1 = ann.arrAnns.zip(dims).forall { case (_ann, dim) => _ann.lsh.dim == dim }
      val cond2 = ann.compMap.isEmpty

      if (!cond1) ko(s"arrAnns: ${ann.arrAnns}")
      else if (!cond2) ko(s"compMap: ${ann.compMap}")
      else ok
    }

  }

}
