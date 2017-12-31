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

  }

}