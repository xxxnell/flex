package sketch.scope.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope._
import sketch.scope.conf.CustomAdaPerSketchConf

class AdaPerSketchSpec extends Specification with ScalaCheck {

  "AdaPerSketch" should {

    "type invariance after update" in {
      implicit val conf: CustomAdaPerSketchConf = SketchConf(
        queueSize = 10,
        cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 20, counterNo = 2
      )
      val sketch: Sketch[Double] = AdaPerSketch.empty[Double]

      sketch.update(1).fold(ko("Exception occurs.")){
        case sketch: AdaPerSketch[Double] => if(sketch.queue.nonEmpty) ok else ko("Empty queue.")
        case _ => ko(s"sketch: $sketch")
      }
    }

    "update" in {
      implicit val conf: CustomAdaPerSketchConf = SketchConf(
        queueSize = 10,
        cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 20, counterNo = 2
      )
      val sketch = AdaPerSketch.empty[Double]

      sketch.update(1).fold(ko("Exception occurs.")){
        case sketch: AdaPerSketch[Double] => if(sketch.queue.nonEmpty) ok else ko("Empty queue.")
        case _ => ko(s"sketch: $sketch")
      }
    }

    "type invariance after rearrange" in {
      implicit val conf: CustomAdaPerSketchConf = SketchConf(
        queueSize = 10,
        cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 20, counterNo = 2
      )
      val sketch = AdaPerSketch.empty[Double]

      (for {
        sketch1 <- sketch.update(1)
        sketch2 <- sketch1.rearrange
      } yield sketch2)
        .fold(ko("Exception occurs.")){
          case sketch: AdaPerSketch[Double] => if(sketch.queue.nonEmpty) ok else ko("Empty queue.")
          case _ => ko(s"sketch: $sketch")
        }
    }

    "empty queue" in {
      val queueSize = 1
      implicit val conf: CustomAdaPerSketchConf = SketchConf(
        queueSize = queueSize,
        cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 20, counterNo = 2
      )
      val sketch = AdaPerSketch.empty[Double]

      val queue = sketch.asInstanceOf[AdaPerSketch[Double]].queue
      if(queue.nonEmpty) ko("Queue of empty sketch have to be empty.") else ok
    }

    "queue io" in {
      val queueSize = 2
      implicit val conf: CustomAdaPerSketchConf = SketchConf(
        queueSize = queueSize,
        cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 20, counterNo = 2
      )
      val sketch = AdaPerSketch.empty[Double]

      (for {
        sketch1 <- sketch.update(1)
        sketch2 <- sketch1.update(2)
        sketch3 <- sketch2.update(3)
        q2 = sketch2.asInstanceOf[AdaPerSketch[Double]].queue
        q3 = sketch3.asInstanceOf[AdaPerSketch[Double]].queue
      } yield (q2, q3))
        .fold(ko("Exception occurs.")){
          case (q2, q3) =>
            if(q3.size != queueSize) ko(s"queue1 size: ${q3.size}.")
            else if(q3.size != q3.size) ko(s"queue1 size(${q3.size}) and queue2 size(${q3.size}) have to equal.")
            else if(q2.headOption == q3.headOption) ko(s"queue1: $q2 == queue2: $q3")
            else ok
        }
    }

  }

}