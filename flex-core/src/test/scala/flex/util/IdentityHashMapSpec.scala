package flex.util

import flex.util.IdentityHashMap.syntax._
import org.specs2.ScalaCheck
import org.specs2.mutable._

class IdentityHashMapSpec extends Specification with ScalaCheck {

  "IdentityHashMap" should {

    "constructor" in {

      "apply(_*)" in {
        val n = 100
        val map = IdentityHashMap((0 until n).zip(0 until n).toList: _*)
        val cond1 = map.size == n

        if (!cond1) ko else ok
      }

    }

    "FalseEquality" in {

      "equals" in {
        val cond1 = !FalseEquality.equals(FalseEquality)

        if (!cond1) ko else ok
      }

      "eq" in {
        val cond1 = FalseEquality.eq(FalseEquality)

        if (!cond1) ko else ok
      }

    }

    "ops" in {

      "add" in {
        val map = IdentityHashMap.empty.add(FalseEquality, 1)
        val cond1 = map.size == 1

        if (!cond1) ko else ok
      }

      "remove" in {
        val map = IdentityHashMap.empty.add(FalseEquality, 1).remove(FalseEquality)
        val cond1 = map.size == 0

        if (!cond1) ko else ok
      }

      "get" in {
        val v = 1
        val map = IdentityHashMap.empty.add(FalseEquality, v).get(FalseEquality)
        val cond1 = map == Option(v)

        if (!cond1) ko else ok
      }

      "updated" in {

        "notExists" in {
          val v = 1
          val map = IdentityHashMap.empty.updated(FalseEquality, v)
          val cond1 = map.get(FalseEquality).contains(v)

          if (!cond1) ko else ok
        }

        "exists" in {
          val (v1, v2) = (1, 2)
          val map = IdentityHashMap.empty.add(FalseEquality, v1).updated(FalseEquality, v2)
          val cond1 = map.get(FalseEquality).contains(v2)

          if (!cond1) ko else ok
        }

      }

      "isEmpty" in {
        val map = IdentityHashMap.empty.add(FalseEquality, 1).remove(FalseEquality)
        val cond1 = map.isEmpty

        if (!cond1) ko else ok
      }

    }

  }

}
