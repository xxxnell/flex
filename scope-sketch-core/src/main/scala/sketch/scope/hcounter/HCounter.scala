package sketch.scope.hcounter

import cats.implicits._
import sketch.scope.counter.{CDim, Counter}
import sketch.scope.hmap.{HDim, Hmap}

/**
  * Licensed by Probe Technology, Inc.
  */
trait HCounter {

  def structure: List[(Hmap, Counter)]

  def sum: Double

}

trait HCounterOps[HC<:HCounter] {

  def update(hc: HCounter, hdim: HDim, count: Double): Option[HCounter] = for {
    updated <- hc.structure.traverse { case (hmap, counter) =>
      for {
        cdim <- hmap.apply(hdim, counter.size)
        counter2 <- counter.update(cdim, count)
      } yield (hmap, counter2)
    }
  } yield HCounter(updated, hc.sum + count)

  def get(hc: HCounter, hdim: HDim): Option[Double] = for {
    counts <- hc.structure.traverse { case (hmap, counter) =>
      for {
        cdim <- hmap(hdim, counter.size)
        aa <- counter.get(cdim)
      } yield aa
    }
  } yield counts.min

  def sum(hc: HCounter): Double = hc.sum

  def count(hc: HCounter, from: HDim, to: HDim): Option[Double] = {
    (from to to).toList
      .traverse(hdim => get(hc, hdim))
      .map(_.sum)
  }

  def depth(hc: HCounter): Int = hc.structure.size

  def width(hc: HCounter): Int = hc.structure.headOption.fold(0){ case (_, counter) => counter.size }

}

trait HCounterSyntax {

  implicit class HCounterSyntaxImpl(hcounter: HCounter) {
    def update(hdim: HDim, count: Double): Option[HCounter] = HCounter.update(hcounter, hdim, count)
    def get(hdim: HDim): Option[Double] = HCounter.get(hcounter, hdim)
    def sum: Double = HCounter.sum(hcounter)
    def count(from: HDim, to: HDim): Option[Double] = HCounter.count(hcounter, from, to)
    def depth: Int = HCounter.depth(hcounter)
    def width: Int = HCounter.width(hcounter)
  }

}

object HCounter extends HCounterOps[HCounter] { self =>

  private case class HCounterImpl(structure: List[(Hmap, Counter)], sum: Double) extends HCounter

  def apply(structure: List[(Hmap, Counter)], sum: Double): HCounter = HCounterImpl(structure, sum)

//  def structure(structure: List[(Hmap, Counter)]): HCounter = HCounterImpl(structure)

  /**
    * @param width Cdim size of the counter
    * */
  def empty(depth: Int, width: Int): HCounter =
    HCounterImpl((0 until depth).toList.map(i => (Hmap(i), Counter.empty(width))), 0)

}