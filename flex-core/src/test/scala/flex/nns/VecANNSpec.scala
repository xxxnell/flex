package flex.nns

import flex.nns.ANN.syntax._
import flex.rand._
import flex.vec.Vec
import org.specs2.ScalaCheck
import org.specs2.mutable._
import flex.util.IdentityHashMap
import flex.util.IdentityHashMap.syntax._
import flex.util.IdentityHashSet
import flex.util.IdentityHashSet.syntax._

class VecANNSpec extends Specification with ScalaCheck {

  "VecANN" should {

    "construct" in {
      val (l, dim, rng) = (10, 2, IRng(0))
      val (ann0, _) = VecANN.empty(l, dim, 10, rng)

      val cond1 = ann0.htables.size == l
      val cond2 = ann0.vtables.size == l
      val cond3 = ann0.lsh.size == l

      if (!cond1) ko(s"HTable: ${ann0.htables}")
      else if (!cond2) ko(s"VTables: ${ann0.vtables}")
      else if (!cond3) ko(s"LSH: ${ann0.lsh}")
      else ok
    }

    "ops" in {

      "add" in {

        "basic" in {
          val (l, dim, rng) = (10, 2, IRng(0))
          val v = Vec(1.0, 1.0)
          val (ann0, _) = VecANN.empty(l, dim, 10, rng)
          val ann1 = ann0.add(v)

          val cond1 = ann1.htables.forall(htable => htable.size == 1)
          val cond2 = ann1.vtables.forall(vtable => vtable.size == 1)

          if (!cond1) ko(s"HTable: ${ann0.htables}")
          else if (!cond2) ko(s"VTables: ${ann0.vtables}")
          else ok
        }

        "same elements" in todo

      }

      "remove" in {
        val (l, dim, rng0) = (10, 2, IRng(0))
        val (v, rng1) = Vec.std(2, rng0)
        val (ann0, _) = VecANN.empty(l, dim, 10, rng1)
        val ann1 = ann0.add(v)
        val ann2 = ann1.remove(v)

        val cond1 = ann2.htables.forall(htable => htable.isEmpty)
        val cond2 = ann2.vtables.forall(vtable => vtable.isEmpty)

        if (!cond1) ko(s"HTable: ${ann0.htables}")
        else if (!cond2) ko(s"VTables: ${ann0.vtables}")
        else ok
      }

      "search" in {
        val (l, dim, rng) = (10, 2, IRng(0))
        val v = Vec(1.0, 1.0)
        val (ann0, _) = VecANN.empty(l, dim, 10, rng)
        val ann1 = ann0.add(v)
        val search = ann1.search(v)

        val cond1 = search.contains(v)

        if (!cond1) ko
        else ok
      }

    }

  }

}
