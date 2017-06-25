package sketch.scope.hcounter

import cats.implicits._
import sketch.scope.counter.{CDim, Counter}
import sketch.scope.hmap.{HDim, Hmap}

/**
  * Licensed by Probe Technology, Inc.
  */
trait HCounter {

  def structure: List[(Hmap, Counter)]

}

trait HCounterOps[HC<:HCounter] {

  def update(hc: HCounter, hdim: HDim, count: Double): Option[HCounter] = for {
    updated <- hc.structure.traverse { case (hmap, counter) =>
      for {
        cdim <- hmap.apply(hdim, counter.size)
        counter2 <- counter.update(cdim, count)
      } yield (hmap, counter2)
    }
  } yield HCounter(updated)

  def get(hc: HCounter, hdim: HDim): Option[Double] = for {
    counts <- hc.structure.traverse { case (hmap, counter) =>
      for {
        cdim <- hmap(hdim, counter.size)
        aa <- counter.get(cdim)
      } yield aa
    }
  } yield counts.min

}

trait HCounterSyntax {

  implicit class HCounterSyntaxImpl(hcounter: HCounter) {
    def update(hdim: HDim, count: Double): Option[HCounter] = HCounter.update(hcounter, hdim, count)
    def get(hdim: HDim): Option[Double] = HCounter.get(hcounter, hdim)
  }

}

object HCounter extends HCounterOps[HCounter] { self =>

  private case class HCounterImpl(structure: List[(Hmap, Counter)]) extends HCounter

  def apply(structure: List[(Hmap, Counter)]): HCounter = self.structure(structure)

  def structure(structure: List[(Hmap, Counter)]): HCounter = HCounterImpl(structure)

  def empty(depth: Int, cdimSize: Int): HCounter =
    HCounterImpl((0 until depth).toList.map(i => (Hmap(i), Counter.empty(cdimSize))))

}