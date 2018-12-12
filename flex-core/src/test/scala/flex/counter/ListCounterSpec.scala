package flex.counter

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

class ListCounterSpec extends Specification with ScalaCheck {
  
    "ListCounter" should {

      "update" in {
        (for {
            listcounter <- ListCounterGen.listCounterSample
            updated = ListCounter.update(listcounter, 10, 20.0)
        } yield updated)
          .fold(ko)( _ => ok )
      }

    }
}

object ListCounterGen {

  def listCounterGen: Gen[ListCounter] = for {
    size <- Gen.choose(30, 40)
  } yield ListCounter.empty(size)

  def listCounterSample: Option[ListCounter] = listCounterGen.sample

}