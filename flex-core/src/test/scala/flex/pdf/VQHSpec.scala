package flex.pdf

import flex.util.RandomIdentitySet.syntax._
import flex.util.IdentityHashMap.syntax._
import flex.nns.syntax._
import flex.pdf.VQH.syntax._
import flex.rand._
import flex.vec._

import org.specs2.ScalaCheck
import org.specs2.mutable._

import org.scalactic.Tolerance._
import org.scalactic.Explicitly._
import org.scalactic.TripleEquals._
import org.scalactic.StringNormalizations._

class VQHSpec extends Specification with ScalaCheck {

  "VQH" should {

    "construct" in {

      "basic" in {
        val (dims, k) = (List(1, 2, 3, 4, 5), 10)
        val vqh = VQH.empty(dims, k)

        val cond1 = vqh.cws.isEmpty
        val cond2 = vqh.ns.isEmpty
        val cond3 = vqh.ntot == 0
        val cond4 = vqh.k == k
        val cond5a = vqh.nns.lsh.dim == dims.sum
        val cond5b = vqh.nns.size > 0
        val cond6 = vqh.parnns.arrAnns.zip(dims).forall { case (ann, dim) => ann.lsh.dim == dim }

        if (!cond1) ko(s"cws: ${vqh.cws}")
        else if (!cond2) ko(s"cns: ${vqh.ns}")
        else if (!cond3) ko(s"ntot: ${vqh.ntot}")
        else if (!cond4) ko(s"k: ${vqh.k}")
        else if (!(cond5a && cond5b)) ko(s"cwAnn: ${vqh.nns}")
        else if (!cond6) ko(s"parAnn: ${vqh.parnns}")
        else ok
      }

      "empty with dims=Nil" in {
        val (dims, k) = (List.empty[Int], 10)
        val vqh = VQH.empty(dims, k)

        val cond1 = vqh.cws.isEmpty
        val cond2 = vqh.ns.isEmpty
        val cond3 = vqh.ntot == 0
        val cond4 = vqh.k == k
        val cond5 = vqh.nns.lsh.dim == dims.sum
        val cond6 = vqh.parnns.arrAnns.isEmpty

        if (!cond1) ko(s"cws: ${vqh.cws}")
        else if (!cond2) ko(s"cns: ${vqh.ns}")
        else if (!cond3) ko(s"ntot: ${vqh.ntot}")
        else if (!cond4) ko(s"k: ${vqh.k}")
        else if (!cond5) ko(s"cwAnn: ${vqh.nns}")
        else if (!cond6) ko(s"parAnn: ${vqh.parnns}")
        else ok
      }

      "addDim" in {
        val (dims0, dims1, k) = (List.empty[Int], List(1, 2, 3, 4, 5), 10)
        val vqh = VQH.empty(dims0, k).addStd(dims1)

        val cond1 = vqh.cws.isEmpty
        val cond2 = vqh.ns.isEmpty
        val cond3 = vqh.ntot == 0
        val cond4 = vqh.k == k
        val cond5a = vqh.nns.lsh.dim == dims1.sum
        val cond5b = vqh.nns.size > 0
        val cond6 = vqh.parnns.arrAnns.zip(dims1).forall { case (ann, dim) => ann.lsh.dim == dim }

        if (!cond1) ko(s"cws: ${vqh.cws}")
        else if (!cond2) ko(s"cns: ${vqh.ns}")
        else if (!cond3) ko(s"ntot: ${vqh.ntot}")
        else if (!cond4) ko(s"k: ${vqh.k}")
        else if (!(cond5a && cond5b)) ko(s"cwAnn: ${vqh.nns}")
        else if (!cond6) ko(s"parAnn: ${vqh.parnns}")
        else ok
      }

    }

    "ops" in {

      "add" in {
        val (dims, k) = (List(1, 2, 3, 4, 5), 10)
        val vqh0 = VQH.empty(dims, k)
        val xs = (1 to 10).toList.map(i => (SumVec.std(dims, IRng(i))._1, 1.0f))
        val vqh1 = xs.foldLeft(vqh0) { case (_vqh, (x, w)) => _vqh.add(x, w) }

        val cond1 = vqh1.size == xs.size
        val cond2 = vqh1.last == xs.last._1

        if (!cond1) ko(s"vqh.size: ${vqh1.size}, xs.size: ${xs.size}")
        else if (!cond2) ko(s"vqh1.latest: ${vqh1.last}")
        else ok
      }

      "remove" in {
        val (dims, k) = (List(1, 2, 3, 4, 5), 10)
        val vqh0 = VQH.empty(dims, k)
        val xs = (1 to 10).toList.map(i => (SumVec.std(dims, IRng(0))._1, 1.0f))
        val vqh1 = xs.foldLeft(vqh0) { case (_vqh, (x, w)) => _vqh.add(x, w) }
        val vqh2 = xs.foldLeft(vqh1) { case (_vqh, (x, _)) => _vqh.remove(x) }

        val expected = xs.size - xs.size
        val cond1 = vqh2.size == expected

        if (!cond1) ko(s"vqh.size: ${vqh2.size}, expected: $expected")
        else ok
      }

      "addDim" in {
        val (dims0, k) = (List(1, 2, 3, 4, 5), 10)
        val n = 6
        val dims1 = dims0.:+(n)
        val vqh0 = VQH.empty(dims0, k)
        val vqh1 = vqh0.addDim(List.fill(n)(NormalDist(0.0, 1.0)))

        val cond1 = vqh1.dims == dims1
        val cond2 = vqh1.nns.dims == dims1
        val cond3 = vqh1.parnns.dims == dims1

        if (!cond1) ko(s"vqh.dims: ${vqh1.dims}, expected: $dims1")
        if (!cond2) ko(s"vqh.nns.dims: ${vqh1.nns.dims}, expected: $dims1")
        if (!cond3) ko(s"vqh.parnns.dims: ${vqh1.parnns.dims}, expected: $dims1")
        else ok
      }

      "parUpdate" in {

        "first" in {
//          val (dims, k, n, rng0) = (List(1, 2, 3, 4, 5), 20, 3, IRng(0))
//          val vqh0 = VQH.empty(dims, k)
//          val (irngs, _) = (0 until n).toList.foldLeft((List.empty[(Int, IRng)], rng0)) {
//            case ((_is, _rng1), _) =>
//              val (_rng2, i) = _rng1.next
//              (((i * dims.size).floor.toInt, _rng2) :: _is, _rng2)
//          }
//          val xps = irngs.map { case (i, rng) => (Vec.std(dims(i), rng)._1, i, 1.0f) }
//          val (vqh1, cins, couts) = vqh0.parUpdate(xps, ???)
//
//          val cond1 = vqh1.cws.size == xps.size
//          val cond2 = vqh1.ns.size == xps.size
//          val cond3 = vqh1.ntot === xps.map(_._3).sum +- 0.01f
//          val cond4 = vqh1.parnns.arrAnns.zip(dims).forall {
//            case (ann, dim) => ann.vtables.forall(vt => vt.keySet.forall(v => v.dim == dim))
//          }
//
//          if (!cond1) ko(s"cws: ${vqh1.cws}, \ncins: $cins, \ncouts: $couts")
//          else if (!cond2) ko(s"cns: ${vqh1.ns}, \ncins: $cins, \ncouts: $couts")
//          else if (!cond3) ko(s"ntot: ${vqh1.ntot}")
//          else if (!cond4) ko(s"arbitrary vectors: ${vqh1.parnns.arrAnns.map(ann => ann.vtables.head.keySet)}")
//          else ok

          todo
        }

        "k" in {
//          val (dims, k, n, rng0) = (List(1, 2, 3, 4, 5), 20, 300, IRng(0))
//          val vqh0 = VQH.empty(dims, k)
//          val (irngs, _) = (1 to n).toList.foldLeft((List.empty[(Int, IRng)], rng0)) {
//            case ((_is, _rng1), _) =>
//              val (_rng2, i) = _rng1.next
//              (((i * dims.size).floor.toInt, _rng2) :: _is, _rng2)
//          }
//          val xps = irngs.map { case (i, rng) => (Vec.std(dims(i), rng)._1, i, 1.0f) }
//          val vqh1 = xps.foldLeft(vqh0) { case (_vqh, (x, i, w)) => _vqh.parUpdate((x, i, w) :: Nil, ???)._1 }
//
//          val cond1 = vqh1.size === k +- (k * 0.3).round.toInt
//
//          if (!cond1) ko(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")
//          else ok(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")

          todo
        }

      }

      "expUpdate" in {

        "first" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 20)
          val vqh0 = VQH.empty(dims, k)
          val xs = (1 to 10).toList.map(i => (SumVec.std(dims, IRng(i))._1, 1.0f))
          val (vqh1, cins, couts) = vqh0.expUpdate(xs)

          println(s"${vqh1.ntot} ?= ${xs.map(_._2).sum}")

          val cond1 = vqh1.cws.size == xs.size
          val cond2 = vqh1.ns.size == xs.size
          val cond3 = vqh1.ntot === xs.map(_._2).sum +- 0.01f

          if (!cond1) ko(s"ns: ${vqh1.cws}, \ncins: $cins, \ncouts: $couts")
          else if (!cond2) ko(s"ns: ${vqh1.ns}, \ncins: $cins, \ncouts: $couts")
          else if (!cond3) ko(s"ntot: ${vqh1.ntot}")
          else ok
        }

        "vqh size should be similar to k" in {
          val (dims, k, n) = (List(1, 2, 3, 4, 5), 20, 300)
          val vqh0 = VQH.empty(dims, k)
          val xs = (1 to n).toList.map(i => (SumVec.std(dims, IRng(i))._1, 1.0f))
          val vqh1 = xs.foldLeft(vqh0) {
            case (_vqh, (x, w)) => _vqh.expUpdate((x, w) :: Nil)._1
          }

          val cond1 = vqh1.size === k +- (k * 0.3).round.toInt

          if (!cond1) ko(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")
          else ok(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")
        }

      }

      "parSearch" in {

        "empty" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val i = 0
          val (xp, _) = Vec.std(dims(i), IRng(0))
          val res = vqh0.parSearch(xp, i)

          val cond1 = res.isEmpty

          if (!cond1) ko(s"result: $res")
          else ok
        }

        "basic" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val i = 0
          val (x, _) = SumVec.std(dims, IRng(0))
          val vqh1 = vqh0.add(x, 1.0f)
          val res = vqh1.parSearch(x.apply(i), i)

          val cond1 = res.contains(x)

          if (!cond1) ko(s"result: $res")
          else ok
        }

      }

      "expSearch" in {

        "empty" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val (x, _) = SumVec.std(dims, IRng(0))
          val res = vqh0.expSearch(x)

          val cond1 = res.isEmpty

          if (!cond1) ko(s"result: $res")
          else ok
        }

        "basic" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val (x, _) = SumVec.std(dims, IRng(0))
          val vqh1 = vqh0.add(x, 1.0f)
          val res = vqh1.expSearch(x)

          val cond1 = res.contains(x)

          if (!cond1) ko(s"result: $res")
          else ok
        }

      }

    }

  }

}
