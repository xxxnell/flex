package flip.hcounter

import org.specs2.mutable._
import org.specs2.ScalaCheck

class UncompressedHCounterSpec extends Specification with ScalaCheck {

  "UncompressedHCounter" should {

    "get" in {

      "basic" in {
        val emptyCounter = HCounter.emptyUncompressed(100)
        val datas = (0 until 100).map(i => (i, i * i)).toList
        val utdCounter = datas
          .foldLeft(emptyCounter) { case (accCounter, (idx, count)) => accCounter.update(idx, count)}
        val cond = datas.forall { case (idx, count) => utdCounter.get(idx) == count }

        if(!cond) ko("Somewhere HCounter collides.")
        else ok
      }

    }

  }

}