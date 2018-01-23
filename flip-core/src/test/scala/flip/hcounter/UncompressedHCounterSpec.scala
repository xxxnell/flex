package flip.hcounter

import org.specs2.mutable._
import org.specs2.ScalaCheck

class UncompressedHCounterSpec extends Specification with ScalaCheck {

  "UncompressedHCounter" should {

    "get" in {

      "basic" in {
        val emptyCounter = HCounter.emptyUncompressed(100)
        val utdCounterO = (0 until 100).map(i => (i, i * i))
          .foldLeft(Option(emptyCounter)) { case (accCounterO, (idx, count)) =>
            accCounterO.flatMap(counter => counter.update(idx, count))
          }
        val cond = (0 until 100).forall(i => (for {
          utdCounter <- utdCounterO
          count <- utdCounter.get(i)
        } yield count == i * i).getOrElse(false))

        if(!cond) ko("Somewhere HCounter collides")
        else ok
      }

    }

  }

}