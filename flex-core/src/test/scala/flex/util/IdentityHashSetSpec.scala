package flex.util

import flex.util.IdentityHashSet.syntax._

import org.specs2.mutable._
import org.specs2.ScalaCheck

class IdentityHashSetSpec extends Specification with ScalaCheck {

  "IdentityHashSet" should {

    "add" in {
      val set = IdentityHashSet.empty.add(FalseEquality)
      val cond1 = set.size == 1

      if (!cond1) ko else ok
    }

    "remove" in {
      val set = IdentityHashSet.empty.add(FalseEquality).remove(FalseEquality)
      val cond1 = set.size == 0

      if (!cond1) ko else ok

    }

    "exists" in {
      val set = IdentityHashSet.empty.add(FalseEquality)
      val cond1 = set.exists(FalseEquality)

      if (!cond1) ko else ok
    }

  }

}
