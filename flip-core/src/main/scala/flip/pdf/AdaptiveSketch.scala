package flip.pdf

import flip.conf.{AdaPerSketchConf, AdaptiveSketchConf}
import flip.measure.Measure
import flip.plot.CountPlot

import scala.language.higherKinds

trait AdaptiveSketch[A] extends Sketch[A] {

  val queue: List[(A, Count)]

  def conf: AdaptiveSketchConf

}

trait AdaptiveSketchOps[S[_]<:AdaptiveSketch[_]]
  extends SketchPrimPropOps[S]
    with AdaptiveSketchLaws[S] { self =>

  def modifyQueue[A](sketch: S[A], f: List[(A, Count)] => List[(A, Count)]): S[A]

  // overrides

  def queueCorrection(sketch: S[_]): Double = {
    val cmapNo = sketch.conf.cmap.no
    val decayFactor = sketch.conf.decayFactor
    val effNo = if(cmapNo > 1) cmapNo - 1 else cmapNo

    lazy val effRates = (0 until effNo).map(i => decayRate(decayFactor, i))
    lazy val allRates = (0 until cmapNo).map(i => decayRate(decayFactor, i))

    if(sketch.structures.size < cmapNo) 1 else effRates.sum / allRates.sum
  }

  override def count[A](sketch: S[A], start: A, end: A): Option[Count] = for {
    countStr <- countForStr(sketch, start, end)
    countQ = countForQueue(sketch, start, end)
  } yield countStr + queueCorrection(sketch) * countQ

  override def sum(sketch: S[_]): Count =
    sumForStr(sketch) + queueCorrection(sketch) * sumForQueue(sketch)

  override def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[S[A]] = for {
    (sketch1, old) <- Some(append(sketch, as))
    sketch2 <- super.narrowUpdate(sketch1, old)
  } yield sketch2

  override def rearrange[A](sketch: S[A]): Option[S[A]] = for {
    sketch1OldStr <- deepUpdate(sketch, sketch.queue.asInstanceOf[List[(A, Count)]])
    (sketch1, _) = sketch1OldStr
    sketch2 = clearQueue(sketch1)
  } yield sketch2

}

trait AdaptiveSketchLaws[S[_]<:AdaptiveSketch[_]] { self: AdaptiveSketchOps[S] =>

  def append[A](sketch: S[A], as: List[(A, Count)]): (S[A], List[(A, Count)]) = {
    var oldAs = List.empty[(A, Count)]
    val utdSkt = modifyQueue(sketch, (queue: List[(A, Count)]) => {
      val (utd, old) = (as ++ queue).splitAt(sketch.conf.queueSize)
      oldAs = old
      utd
    })

    (utdSkt, oldAs)
  }

  def clearQueue[A](sketch: S[A]): S[A] = modifyQueue(sketch, _ => List.empty[(A, Count)])

  def countForQueue[A](sketch: S[A], start: A, end: A): Count = {
    val measure: Measure[A] = sketch.measure.asInstanceOf[Measure[A]]
    val startP = measure.to(start)
    val endP = measure.to(end)

    sketch.queue.asInstanceOf[List[(A, Count)]]
      .filter { case (a, _) => measure.to(a) >= startP && measure.to(a) <= endP }
      .map(_._2).sum
  }

  def sumForQueue[A](sketch: S[A]): Count = sketch.queue.foldLeft(0d){ case (acc, (_, count)) => acc + count }

  def pdfForQueue[A](sketch: S[A], a: A): Option[Double] = for {
    cmap <- youngCmap(sketch)
    measure = sketch.measure.asInstanceOf[Measure[A]]
    queue = sketch.queue.asInstanceOf[List[(A, Count)]]
    p = measure.to(a)
    adim = cmap(p)
    filteredQ1 = queue.filter { case (_a, _) => cmap(measure.to(_a)) == adim -1 }
    filteredQ2 = queue.filter { case (_a, _) => cmap(measure.to(_a)) == adim }
    filteredQ3 = queue.filter { case (_a, _) => cmap(measure.to(_a)) == adim + 1 }
    count1 = filteredQ1.foldLeft(0d){ case (acc, (_, _count)) => acc + _count }
    count2 = filteredQ2.foldLeft(0d){ case (acc, (_, _count)) => acc + _count }
    count3 = filteredQ3.foldLeft(0d){ case (acc, (_, _count)) => acc + _count }
    range1 = cmap.range(adim - 1)
    range2 = cmap.range(adim)
    range3 = cmap.range(adim + 1)
    records = (range1, count1) :: (range2, count2) :: (range3, count3) :: Nil
    count = CountPlot.disjoint(records).interpolation(p)
    sum = sumForQueue(sketch)
  } yield {
    if(sum != 0 && !range2.isPoint) (count / (sum * range2.length)).toDouble
    else if(sum == 0) flatDensity
    else if(count == 0) 0
    else Double.PositiveInfinity
  }

}

object AdaptiveSketch extends AdaptiveSketchOps[AdaptiveSketch] {

  def modifyQueue[A](sketch: AdaptiveSketch[A],
                     f: List[(A, Count)] => List[(A, Count)]): AdaptiveSketch[A] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyQueue(sketch, f)
  }

  def modifyStructure[A](sketch: AdaptiveSketch[A],
                         f: Structures => Option[Structures]): Option[AdaptiveSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
  }

  def update[A](sketch: AdaptiveSketch[A],
                as: List[(A, Count)]): Option[AdaptiveSketch[A]] = sketch match {
    case (sketch: AdaPerSketch[A]) => AdaPerSketch.update(sketch, as)
    case _ => narrowUpdate(sketch, as)
  }

}