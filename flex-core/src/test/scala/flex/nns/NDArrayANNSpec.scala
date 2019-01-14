package flex.nns

import flex.rand._
import flex.nns.ANN.syntax._
import org.nd4j.linalg.factory.Nd4j
import org.specs2.mutable._
import org.specs2.ScalaCheck

class NDArrayANNSpec extends Specification with ScalaCheck {

  "NDArrayANN" should {

    "construct" in {
      val (l, dim, rng) = (10, 2, IRng(0))
      val (ann0, _) = NDArrayANN.empty(l, dim, rng)

      val cond1 = ann0.htables.length == l
      val cond2 = ann0.vtables.length == l
      val cond3 = ann0.lshs.length == l

      if (!cond1) ko(s"HTable: ${ann0.htables}")
      else if (!cond2) ko(s"VTables: ${ann0.vtables}")
      else if (!cond3) ko(s"LSH: ${ann0.lshs}")
      else ok
    }

    "ops" in {

      "add" in {

        "basic" in {
          val (l, dim, rng) = (10, 2, IRng(0))
          val v = Nd4j.create(Array(1.0, 1.0))
          val (ann0, _) = NDArrayANN.empty(l, dim, rng)
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
        val (l, dim, rng) = (10, 2, IRng(0))
        val v = Nd4j.create(Array(1.0, 1.0))
        val (ann0, _) = NDArrayANN.empty(l, dim, rng)
        val ann1 = ann0.add(v)
        val ann2 = ann0.remove(v)

        val cond1 = ann2.htables.forall(htable => htable.isEmpty)
        val cond2 = ann2.vtables.forall(vtable => vtable.isEmpty)

        if (!cond1) ko(s"HTable: ${ann0.htables}")
        else if (!cond2) ko(s"VTables: ${ann0.vtables}")
        else ok
      }

      "search" in {
        val (l, dim, rng) = (10, 2, IRng(0))
        val v = Nd4j.create(Array(1.0, 1.0))
        val (ann0, _) = NDArrayANN.empty(l, dim, rng)
        val ann1 = ann0.add(v)
        val search = ann1.search(v)

        val cond1 = search.contains(v)

        if (!cond1) ko
        else ok
      }

    }

  }

}
