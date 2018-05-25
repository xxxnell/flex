package flip.hcounter

import org.specs2.mutable._
import org.specs2.ScalaCheck

class UncompressedHCounterSpec extends Specification with ScalaCheck {

  "UncompressedHCounter" should {

    "get" in {

      "basic" in {
        val counter0 = HCounter.emptyUncompressed(100)
        val datas = (0 until 100).map(i => (i, i * i)).toList
        val counter1 = datas
          .foldLeft(counter0) { case (accCounter, (idx, count)) => accCounter.update(idx, count)}
        val cond = datas.forall { case (idx, count) => counter1.get(idx) == count }

        if(!cond) ko("Somewhere HCounter collides.")
        else ok
      }

    }

  }

}