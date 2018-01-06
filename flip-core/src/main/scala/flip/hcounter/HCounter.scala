package flip.hcounter

import cats.implicits._
import flip.conf.CounterConf
import flip.counter.{CDim, Counter}
import flip.hcounter.HCounter.width
import flip.hmap.{HDim, Hmap}
import flip.pdf.Count

import scala.util.hashing.byteswap32

trait HCounter {

  def structures: List[(Hmap, Counter)]

  def sum: Double

  override def toString: String = {
    val structuresStr = structures.map { case (hmap, counter) => s"($hmap, $counter)" }.mkString(", ")
    s"HCounter($structuresStr)"
  }

}

trait HCounterOps[HC<:HCounter] {

  def update(hc: HCounter, hdim: HDim, count: Double): Option[HCounter] = for {
    updated <- hc.structures.traverse { case (hmap, counter) =>
      for {
        cdim <- hmap.apply(hdim, counter.size)
        counter2 <- counter.update(cdim, count)
      } yield (hmap, counter2)
    }
  } yield HCounter(updated, hc.sum + count)

  def updates(hc: HCounter, as: List[(HDim, Count)]): Option[HCounter] =
    as.foldLeft(Option(hc)) { case (hcO, (hdim, count)) => hcO.flatMap(hc => hc.update(hdim, count)) }

  def get(hc: HCounter, hdim: HDim): Option[Double] = for {
    counts <- hc.structures.traverse { case (hmap, counter) =>
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

  def depth(hc: HCounter): Int = hc.structures.size

  def width(hc: HCounter): Int = hc.structures.headOption.fold(0){ case (_, counter) => counter.size }

}

trait HCounterSyntax {

  implicit class HCounterSyntaxImpl(hcounter: HCounter) {
    def update(hdim: HDim, count: Count): Option[HCounter] = HCounter.update(hcounter, hdim, count)
    def updates(as: List[(HDim, Count)]): Option[HCounter] = HCounter.updates(hcounter, as)
    def get(hdim: HDim): Option[Double] = HCounter.get(hcounter, hdim)
    def sum: Double = HCounter.sum(hcounter)
    def count(from: HDim, to: HDim): Option[Double] = HCounter.count(hcounter, from, to)
    def depth: Int = HCounter.depth(hcounter)
    def width: Int = HCounter.width(hcounter)
  }

}

object HCounter extends HCounterOps[HCounter] { self =>

  private case class HCounterImpl(structures: List[(Hmap, Counter)], sum: Double) extends HCounter

  def apply(structure: List[(Hmap, Counter)], sum: Double): HCounter = bare(structure, sum)

  def apply(conf: CounterConf, seed: Int): HCounter = emptyForConf(conf, seed)

  def bare(structures: List[(Hmap, Counter)], sum: Double): HCounter = HCounterImpl(structures, sum)

  /**
    * @param width Cdim size of the counter
    * */
  def empty(depth: Int, width: Int, seed: Int): HCounter = {
    val hmapSeed: Int => Int = (i: Int) => byteswap32(seed ^ Int.MaxValue) << i
    val strs = (0 until depth).toList.map(i => (Hmap(hmapSeed(i)), Counter.empty(width)))
    bare(strs, 0)
  }

  def emptyForConf(conf: CounterConf, seed: Int): HCounter = empty(conf.no, conf.size, seed)

}