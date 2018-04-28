package flip.pdf

import flip.conf.{AdaPerSketchConf, AdaptiveSketchConf}
import flip.measure.Measure
import flip.plot.CountPlot
import flip.rand.IRng
import flip.pdf.Buffer.syntax._

import scala.language.higherKinds
import cats.implicits._

import scala.collection.immutable.Queue

/**
  * Adaptive Sketch has its own buffer to hold recent input streams temporarily.
  * This buffer retains its maximum length.
  * The buffer acts as a reference data stream ξ for deepUpdate when Adaptive
  * Sketch has to rearrange. So, even if the concept drift occurs, rearranging
  * can constitute the correct strueture of Sketch.
  *
  * rearrange(S) = deepUpdate(S, ξ)
  *  where S is sketch and ξ is recent data stream in the buffer of Adaptive
  *  Sketch.
  * */
trait AdaptiveSketch[A] extends Sketch[A] {

  val buffer: Buffer[A]

  def conf: AdaptiveSketchConf

}

trait AdaptiveSketchOps[S[_] <: AdaptiveSketch[_]] extends SketchPrimPropOps[S] with AdaptiveSketchLaws[S] { self =>

  def modifyBuffer[A](sketch: S[A], f: Buffer[A] => Buffer[A]): S[A]

  // overrides

  def queueCorrection(sketch: S[_]): Double = {
    val cmapNo = sketch.conf.cmap.no
    val decayFactor = sketch.conf.decayFactor
    val effNo = if (cmapNo > 1) cmapNo - 1 else cmapNo

    lazy val effRates = (0 until effNo).map(i => decayRate(decayFactor, i))
    lazy val allRates = (0 until cmapNo).map(i => decayRate(decayFactor, i))

    if (sketch.structures.size < cmapNo) 1 else effRates.sum / allRates.sum
  }

  override def count[A](sketch: S[A], start: A, end: A): Count = {
    val countStr = countForStr(sketch, start, end)
    val countQ = countForQueue(sketch, start, end)
    countStr + queueCorrection(sketch) * countQ
  }

  override def sum(sketch: S[_]): Count = {
    sumForStr(sketch) + queueCorrection(sketch) * sumForQueue(sketch)
  }

  override def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): S[A] = {
    val (sketch1, old) = append(sketch, as)
    super.narrowUpdate(sketch1, old)
  }

  override def rearrange[A](sketch: S[A]): S[A] = {
    val (sketch1, _) = deepUpdate(sketch, sketch.buffer.asInstanceOf[List[(A, Count)]])
    clearBuffer(sketch1)
  }

}

trait AdaptiveSketchLaws[S[_] <: AdaptiveSketch[_]] { self: AdaptiveSketchOps[S] =>

  def append[A](sketch0: S[A], as: List[(A, Count)]): (S[A], List[(A, Count)]) = {
    import flip._

    var rem: Buffer[A] = null
    val sketch1 = modifyBuffer(
      sketch0,
      (buffer0: Buffer[A]) => {
        val concat = buffer0 ++ as
        val overflow = concat.size - sketch0.conf.bufferSize
        val (_rem, buffer1) = concat.splitAt(overflow)
        rem = _rem
        buffer1
      }
    )

    (sketch1, rem.toList)
  }

  def clearBuffer[A](sketch: S[A]): S[A] = modifyBuffer(sketch, _ => Buffer.empty[A])

  def countForQueue[A](sketch: S[A], start: A, end: A): Count = {
    val measure: Measure[A] = sketch.measure.asInstanceOf[Measure[A]]
    val startP = measure.to(start)
    val endP = measure.to(end)

    sketch.buffer
      .asInstanceOf[Buffer[A]]
      .toList
      .filter { case (a, _) => measure.to(a) >= startP && measure.to(a) <= endP }
      .map(_._2)
      .sum
  }

  def sumForQueue[A](sketch: S[A]): Count = sketch.buffer.sum

  def pdfForQueue[A](sketch: S[A], a: A): Double = {
    val cmap = youngCmap(sketch)
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    val queue = sketch.buffer.asInstanceOf[List[(A, Count)]]
    val p = measure.to(a)
    val adim = cmap(p)
    val filteredQ1 = queue.filter { case (_a, _) => cmap(measure.to(_a)) == adim - 1 }
    val filteredQ2 = queue.filter { case (_a, _) => cmap(measure.to(_a)) == adim }
    val filteredQ3 = queue.filter { case (_a, _) => cmap(measure.to(_a)) == adim + 1 }
    val count1 = filteredQ1.foldLeft(0.0) { case (acc, (_, _count)) => acc + _count }
    val count2 = filteredQ2.foldLeft(0.0) { case (acc, (_, _count)) => acc + _count }
    val count3 = filteredQ3.foldLeft(0.0) { case (acc, (_, _count)) => acc + _count }
    val range1 = cmap.range(adim - 1)
    val range2 = cmap.range(adim)
    val range3 = cmap.range(adim + 1)
    val records = (range1, count1) :: (range2, count2) :: (range3, count3) :: Nil
    val count = CountPlot.disjoint(records).interpolation(p)
    val sum = sumForQueue(sketch)

    if (sum != 0 && !range2.isPoint) (count / (sum * range2.length)).toDouble
    else if (sum == 0) flatDensity
    else if (count == 0) 0
    else Double.PositiveInfinity
  }

}

object AdaptiveSketch extends AdaptiveSketchOps[AdaptiveSketch] {

  def modifyRng[A](dist: AdaptiveSketch[A], f: IRng => IRng): AdaptiveSketch[A] = ???

  def modifyBuffer[A](sketch: AdaptiveSketch[A], f: Buffer[A] => Buffer[A]): AdaptiveSketch[A] =
    sketch match {
      case sketch: AdaPerSketch[A] => AdaPerSketch.modifyBuffer(sketch, f)
    }

  def modifyStructure[A](sketch: AdaptiveSketch[A], f: Structures => Structures): AdaptiveSketch[A] =
    sketch match {
      case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
    }

  def update[A](sketch: AdaptiveSketch[A], as: List[(A, Count)]): AdaptiveSketch[A] = sketch match {
    case (sketch: AdaPerSketch[A]) => AdaPerSketch.update(sketch, as)
    case _ => narrowUpdate(sketch, as)
  }

}
