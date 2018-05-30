package flip.hcounter

import cats.data.NonEmptyList
import flip.counter.Counter
import flip.hmap.{HDim, Hmap}
import flip.pdf.Count

import scala.util.hashing.byteswap32
import cats.implicits._
import flip.conf.counter.CounterConf

/**
  * The HCounter, or Hashing Counter, is a probabilistic data structure that
  * uses only low memory to count the number of finite countable elements. This
  * structure is similar to the Count-min Sketch algorithm.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch"></a>
  * */
trait HCounter {

  def structures: NonEmptyList[(Hmap, Counter)]

  private[hcounter] lazy val structuresArr = structures.toList.toArray

  def sum: Double

  override def toString: String = {
    val structuresStr = structures.toList.map { case (hmap, counter) => s"($hmap, $counter)" }.mkString(", ")
    s"HCounter($structuresStr)"
  }

}

trait HCounterOps[HC <: HCounter] {

  def update(hc: HCounter, hdim: HDim, count: Double): HCounter = {
    val updated = hc.structures.map {
      case (hmap, counter) =>
        val cdim = hmap.apply(hdim, counter.size)
        val utdCounter = counter.update(cdim, count)
        (hmap, utdCounter)
    }

    HCounter(updated, hc.sum + count)
  }

  def updates(hc: HCounter, as: List[(HDim, Count)]): HCounter = {
    as.foldLeft(hc) { case (hcAcc, (hdim, count)) => hcAcc.update(hdim, count) }
  }

  def get(hc: HCounter, hdim: HDim): Double = {
    var i = 0
    var min = Double.MaxValue
    val strs = hc.structuresArr
    while (i < strs.length) {
      val (hmap, counter) = strs.apply(i)
      val cdim = hmap.apply(hdim, counter.counts.size)
      val count = Counter.get(counter, cdim)
      min = if (min < count) min else count
      i += 1
    }
    min
  }

  def sum(hc: HCounter): Double = hc.sum

  def count(hc: HCounter, start: HDim, end: HDim): Double = {
    var hdim = start
    var sum = 0.0
    while (hdim <= end) {
      sum += get(hc, hdim)
      hdim += 1
    }
    sum
  }

  def depth(hc: HCounter): Int = hc.structures.size.toInt

  def width(hc: HCounter): Int = hc.structures.head._2.size

}

trait HCounterSyntax {

  implicit class HCounterSyntaxImpl(hcounter: HCounter) {
    def update(hdim: HDim, count: Count): HCounter = HCounter.update(hcounter, hdim, count)
    def updates(as: List[(HDim, Count)]): HCounter = HCounter.updates(hcounter, as)
    def get(hdim: HDim): Double = HCounter.get(hcounter, hdim)
    def sum: Double = HCounter.sum(hcounter)
    def count(from: HDim, to: HDim): Double = HCounter.count(hcounter, from, to)
    def depth: Int = HCounter.depth(hcounter)
    def width: Int = HCounter.width(hcounter)
  }

}

object HCounter extends HCounterOps[HCounter] { self =>

  private case class HCounterImpl(structures: NonEmptyList[(Hmap, Counter)], sum: Double) extends HCounter

  def apply(structure: NonEmptyList[(Hmap, Counter)], sum: Double): HCounter = bare(structure, sum)

  def apply(conf: CounterConf, seed: Int): HCounter = emptyForConf(conf, seed)

  def bare(structures: NonEmptyList[(Hmap, Counter)], sum: Double): HCounter = HCounterImpl(structures, sum)

  /**
    * @param width Cdim size of the counter
    * */
  def empty(depth: Int, width: Int, seed: Int): HCounter = {
    val hmapSeed: Int => Int = (i: Int) => byteswap32(seed ^ Int.MaxValue) << i
    val strs = NonEmptyList.fromListUnsafe((0 until depth).toList).map(i => (Hmap(hmapSeed(i)), Counter.empty(width)))

    bare(strs, 0)
  }

  def emptyForConf(conf: CounterConf, seed: Int): HCounter = empty(conf.no, conf.size, seed)

  def emptyUncompressed(size: Int): HCounter = {
    val strs = NonEmptyList.of((Hmap.identity, Counter.empty(size)))

    bare(strs, 0)
  }

}
