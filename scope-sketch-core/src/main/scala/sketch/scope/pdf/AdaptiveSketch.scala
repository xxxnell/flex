package sketch.scope.pdf

import sketch.scope.conf.{AdaPerSketchConf, AdaptiveSketchConf}
import sketch.scope.measure.Measure

import scala.language.higherKinds

trait AdaptiveSketch[A] extends Sketch[A] {

  val queue: List[(A, Count)]

}

trait AdaptiveSketchOps[S[_]<:AdaptiveSketch[_], C<:AdaptiveSketchConf]
  extends SketchPrimPropOps[S, C]
    with AdaptiveSketchLaws[S, C] { self =>

  def modifyQueue[A](sketch: S[A], f: List[(A, Count)] => List[(A, Count)]): S[A]

  // overrides

  override def count[A](sketch: S[A], start: A, end: A): Option[Count] = for {
    countStr <- countForStr(sketch, start, end)
    countQ = countForQueue(sketch, start, end)
  } yield countStr + countQ

  override def sum(sketch: S[_]): Count = sumForStr(sketch) + sumForQueue(sketch)

  override def narrowUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[S[A]] = for {
    (sketch1, old) <- Some(append(sketch, as, conf))
    sketch2 <- super.narrowUpdate(sketch1, old, conf)
  } yield sketch2

  override def rearrange[A](sketch: S[A], conf: C): Option[S[A]] = for {
    sketch1OldStr <- deepUpdate(sketch, sketch.queue.asInstanceOf[List[(A, Count)]], conf)
    (sketch1, _) = sketch1OldStr
    sketch2 = clearQueue(sketch1)
  } yield sketch2

  override def fastPdf[A](sketch: S[A], a: A): Option[Count] = for {
    pdfNStr <- pdfNForStr(sketch, a)
    pdfQ <- pdfForQueue(sketch, a)
    pdfNQ = pdfQ * sumForQueue(sketch)
    sum = self.sum(sketch)
  } yield (pdfNStr + pdfNQ) / sum

}

trait AdaptiveSketchLaws[S[_]<:AdaptiveSketch[_], C<:AdaptiveSketchConf] { self: AdaptiveSketchOps[S, C] =>

  def append[A](sketch: S[A], as: List[(A, Count)], conf: C): (S[A], List[(A, Count)]) = {
    var oldAs = List.empty[(A, Count)]
    val utdSkt = modifyQueue(sketch, (queue: List[(A, Count)]) => {
      val (utd, old) = (as ++ queue).splitAt(conf.queueSize)
      oldAs = old
      utd
    })

    (utdSkt, oldAs)
  }

  def clearQueue[A](sketch: S[A]): S[A] = modifyQueue(sketch, _ => List.empty[(A, Count)])

  def countForQueue[A](sketch: S[A], start: A, end: A): Count = {
    val measure: Measure[A] = sketch.measure.asInstanceOf[Measure[A]]

    sketch.queue.filter { case (a: A, _) => measure.to(a) >= measure.to(start) && measure.to(a) <= measure.to(end) }
      .foldLeft(0d){ case (acc, (_, count)) => acc + count }
  }

  def sumForQueue[A](sketch: S[A]): Count = sketch.queue.foldLeft(0d){ case (acc, (_, count)) => acc + count }

  def pdfForQueue[A](sketch: S[A], a: A): Option[Double] = for {
    cmap <- youngCmap(sketch)
    measure = sketch.measure.asInstanceOf[Measure[A]]
    adim = cmap(measure.to(a))
    queue = sketch.queue.asInstanceOf[List[(A, Count)]]
    filteredQ = queue.filter { case (_a, _) => cmap(measure.to(_a)) == adim }
    count = filteredQ.foldLeft(0d){ case (acc, (_, _count)) => acc + _count }
    range = cmap.range(adim)
  } yield if(range.length != 0) (count / range.length).toDouble else Double.PositiveInfinity

}

object AdaptiveSketch extends AdaptiveSketchOps[AdaptiveSketch, AdaptiveSketchConf] {

  def modifyQueue[A](sketch: AdaptiveSketch[A],
                     f: List[(A, Count)] => List[(A, Count)]): AdaptiveSketch[A] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyQueue(sketch, f)
  }

  def modifyStructure[A](sketch: AdaptiveSketch[A],
                         f: Structures => Option[Structures]): Option[AdaptiveSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
  }

  def update[A](sketch: AdaptiveSketch[A],
                as: List[(A, Count)],
                conf: AdaptiveSketchConf): Option[AdaptiveSketch[A]] = (sketch, conf) match {
    case (sketch: AdaPerSketch[A], conf: AdaPerSketchConf) => AdaPerSketch.update(sketch, as, conf)
    case _ => narrowUpdate(sketch, as, conf)
  }

}