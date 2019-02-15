package flex.chain

import flex.rand.IRng
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flex.chain.RandomSet.syntax._

class RandomSetSpec extends Specification with ScalaCheck {

  "RandomSet" should {

    "construct" in {
      val as = RandomSet.empty[Int](IRng(0))

      val cond1 = as.as.isEmpty
      val cond2 = as.is.isEmpty

      if (!cond1) ko(s"as: ${as.as}")
      else if (!cond2) ko(s"is: ${as.is}")
      else ok
    }

    "add" in {

      "basic" in {
        val as0 = RandomSet.empty[Int](IRng(0))
        val as1 = as0.add(1)

        val cond1 = as1.as.size == 1
        val cond2 = as1.as.size == 1

        if (!cond1) ko(s"as: ${as1.as}")
        else if (!cond2) ko(s"is: ${as1.is}")
        else ok
      }

      "duplicated" in {
        val as0 = RandomSet.empty[Int](IRng(0))
        val as1 = as0.add(1)
        val as2 = as1.add(1)

        val cond1 = as2.as.size == 1
        val cond2 = as2.as.size == 1

        if (!cond1) ko(s"as: ${as2.as}")
        else if (!cond2) ko(s"is: ${as2.is}")
        else ok
      }

    }

    "remove" in {
      val as0 = RandomSet.empty[Int](IRng(0))
      val a = 1
      val as1 = as0.add(a)
      val as2 = as1.remove(a)

      val cond1 = as2.as.isEmpty
      val cond2 = as2.as.isEmpty

      if (!cond1) ko(s"as: ${as2.as}")
      else if (!cond2) ko(s"is: ${as2.is}")
      else ok
    }

    "rand" in {

      "single" in {
        val as0 = RandomSet.empty[Int](IRng(0))
        val a = 1
        val as1 = as0.add(a)
        val (as2, rnd1) = as1.rand
        val (as3, rnd2) = as2.rand

        val cond1 = rnd1.contains(a)
        val cond2 = rnd2.contains(a)
        val cond3 = as2.rng != as3.rng

        if (!cond1) ko(s"first rand: $rnd1")
        else if (!cond2) ko(s"second rand: $rnd2")
        else if (!cond3) ko(s"as2.rng: ${as2.rng}, as3.rng: ${as3.rng}")
        else ok
      }

      "empty" in {
        val as0 = RandomSet.empty[Int](IRng(0))
        val (_, rnd1) = as0.rand

        val cond1 = rnd1.isEmpty

        if (!cond1) ko(s"rnd1: $rnd1")
        else ok
      }

    }

  }

}
