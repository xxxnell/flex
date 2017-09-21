package sketch.scope.hcounter

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.counter.CounterGen

/**
  * Licensed by Probe Technology, Inc.
  */
class HCounterSpec extends Specification with ScalaCheck {

  "HCounter" should {

    "ops" in {

      "update" in {

        "normal" in {
          implicit val hcounterGen = HCounterGen.hcounterA

          prop { (hcounter: HCounter) =>
            (for {
              updatedHcounter <- hcounter.update(10, 20)
              test = updatedHcounter.sum == (hcounter.sum + 20)
            } yield test)
              .fold(ko)(test => if (test) ok else ko)
            //          hcounter.update(10, 20).map(hcounter => { test condition }).fold()()
            //          for {
            //            a <- A
            //            b <- B
            //            c <- C
            //          } yield c
            //          A.flatMap(a => B.flatMap(b => C.map(c )))
          }.setArbitrary(hcounterGen)
        }

        "irregular index" in todo

      }

      "get" in {

        "normal" in {
          implicit val hcounterGen = HCounterGen.hcounterA

          prop { (hcounter: HCounter) =>
            (for {
              updatedHcounter <- hcounter.update(10, 20)
              value <- updatedHcounter.get(10)
            } yield value == 20)
              .fold(ko)(test => if(test) ok else ko )

          }.setArbitrary(hcounterGen)
        }

        "irregular index" in todo

      }

    }

  }

}

object HCounterGen {

  def hcounterGen: Gen[HCounter] = for {
    depth <- Gen.choose(1, 10)
    width <- Gen.choose(100, 10000)
  } yield HCounter.empty(depth, width)

  def hcounterA: Arbitrary[HCounter] = Arbitrary(hcounterGen)

}