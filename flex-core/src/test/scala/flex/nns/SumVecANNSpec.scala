package flex.nns

import flex.rand.IRng
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flex.nns.syntax._
import flex.vec._
import flex.util.IdentityHashMap
import flex.util.IdentityHashMap.syntax._
import flex.util.IdentityHashSet
import flex.util.IdentityHashSet.syntax._

class SumVecANNSpec extends Specification with ScalaCheck {

  "SumVecANN" should {

    "construct" in {
      val (l, dims, rng) = (5, 1 :: 2 :: 3 :: Nil, IRng(0))
      val (ann, _) = SumVecANN.empty(l, dims, rng)

      val cond1a = ann.lsh.size == l
      val cond1b = ann.lsh.dim == dims.sum
      val cond2 = ann.htables.size == l
      val cond3 = ann.vtables.size == l

      if (!(cond1a && cond1b)) ko(s"lshs: ${ann.lsh}")
      else if (!cond2) ko(s"htables: ${ann.htables}")
      else if (!cond3) ko(s"vtables: ${ann.vtables}")
      else ok
    }

    "ops" in {

      "add" in {
        val (l, dims, rng, n) = (5, 1 :: 2 :: 3 :: Nil, IRng(0), 100)
        val (ann0, _) = SumVecANN.empty(l, dims, rng)
        val xs = (1 to n).toList.map(i => SumVec.std(dims, IRng(i))._1)
        val ann1 = xs.foldLeft(ann0) { case (_ann, x) => _ann.add(x) }

        val cond1 = ann1.vtables.forall(vtable => vtable.size == xs.size)

        if (!cond1) ko(s"ann1.vtables.size: ${ann1.vtables.map(_.size)}, expected: ${xs.size}")
        else ok
      }

      "remove" in {
        val (l, dims, rng, n) = (5, 1 :: 2 :: 3 :: Nil, IRng(0), 100)
        val (ann0, _) = SumVecANN.empty(l, dims, rng)
        val xs = (1 to n).toList.map(i => SumVec.std(dims, IRng(i))._1)
        val ann1 = xs.foldLeft(ann0) { case (_ann, x) => _ann.add(x) }
        val ann2 = xs.foldLeft(ann1) { case (_ann, x) => _ann.remove(x) }

        val expected = xs.size - xs.size
        val cond1 = ann2.vtables.forall(vtable => vtable.size == expected)

        if (!cond1) ko(s"ann1.vtables.size: ${ann2.vtables.size}, expected: $expected")
        else ok
      }

      "search" in {

        "basic" in {
          val (l, dims, rng0, n) = (5, 1 :: 2 :: 3 :: Nil, IRng(0), 100)
          val (ann0, rng1) = SumVecANN.empty(l, dims, rng0)
          val xs = (1 to n).toList.map(i => SumVec.std(dims, IRng(i))._1)
          val (query, _) = SumVec.std(dims, rng1)
          val ann1 = xs.foldLeft(ann0) { case (_ann, x) => _ann.add(x) }
          val search = ann1.search(query)

          val cond1 = search.nonEmpty

          if (!cond1) ko(s"ann.isEmpty: ${ann1.isEmpty}, search result: $search")
          else ok
        }

      }

    }

  }

}
