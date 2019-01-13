package flex.nns

import flex.rand.IRng
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flex.nns.syntax._

class CodewordANNSpec extends Specification with ScalaCheck {

  "CodewordANN" should {

    "construct" in {
      val (l, dims, rng) = (10, 1 :: 2 :: 3 :: Nil, IRng(0))
      val (ann, _) = CodewordANN.empty(l, dims, rng)

      val cond1a = ann.lshs.size == l
      val cond1b = ann.lshs.forall(lsh => lsh.dim == dims.sum)
      val cond2 = ann.htables.size == l
      val cond3 = ann.vtables.size == l

      if (!(cond1a && cond1b)) ko(s"lshs: ${ann.lshs}")
      else if (!cond2) ko(s"htables: ${ann.htables}")
      else if (!cond3) ko(s"vtables: ${ann.vtables}")
      else ok
    }

    "ops" in todo

  }

}
