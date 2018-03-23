package flip.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip._
import flip.conf.{AdaPerSketchConf, CustomAdaPerSketchConf, CustomSketchConf}
import flip.measure.syntax._

import scala.language.postfixOps

class AdaPerSketchSpec extends Specification with ScalaCheck {

  "AdaPerSketch" should {

    "type invariance" in {

      "type invariance after update" in {
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = 10,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0: Sketch[Double] = AdaPerSketch.empty[Double]
        val sketch1 = sketch0.update(1)

        if(!sketch1.isInstanceOf[AdaPerSketch[Double]]) ko
        else if(sketch1.asInstanceOf[AdaPerSketch[Double]].queue.isEmpty) ko("Queue is not cleaned.")
        else ok
      }

      "type invariance after rearrange" in {
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = 10,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch = AdaPerSketch.empty[Double]
        val sketch1 = sketch.update(1)
        val sketch2 = sketch1.rearrange

        if(!sketch2.isInstanceOf[AdaPerSketch[Double]]) ko
        else if(sketch2.asInstanceOf[AdaPerSketch[Double]].queue.nonEmpty) ko("Queue is not cleaned.")
        else ok
      }

    }

    "queue" in {

      "empty queue" in {
        val queueSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
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
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch = AdaPerSketch.empty[Double]

        val sketch1 = sketch.update(1)
        val sketch2 = sketch1.update(2)
        val sketch3 = sketch2.update(3)
        val q2 = sketch2.asInstanceOf[AdaPerSketch[Double]].queue
        val q3 = sketch3.asInstanceOf[AdaPerSketch[Double]].queue

        if(q3.size != queueSize) ko(s"queue1 size: ${q3.size}.")
        else if(q3.size != q3.size) ko(s"queue1 size(${q3.size}) and queue2 size(${q3.size}) have to equal.")
        else if(q2.headOption == q3.headOption) ko(s"queue1: $q2 == queue2: $q3")
        else ok
      }

    }

    "count" in {

      "queue only" in {
        val queueSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val count = sketch1.count(1, 1)

        if(count <= 0) ko(s"Counts nothing: $count") else ok
      }

      "queue and structure" in {
        val queueSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val sketch2 = sketch1.update(1)
        val count = sketch2.count(0.9, 1.1)

        if(count <= 1) ko(s"Counts nothing: $count") else ok
      }

    }

    "sum" in {

      "queue only" in {
        val queueSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val sum = sketch1.sum

        if(sum < 1) ko(s"Counts nothing: $sum") else ok
      }

      "queue and structure" in {
        val queueSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val sketch2 = sketch1.update(1)
        val sum = sketch2.sum

        if(sum < 2) ko(s"Counts nothing: $sum") else ok
      }

    }

    "narrowUpdate" in {
      implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
        cmapSize = 2000, cmapNo = 20, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 1000, counterNo = 10
      )
      val sketch0 = AdaPerSketch.empty[Double]

      sketch0.narrowUpdate(0)
      ok
    }

    "deepUpdate" in {
      implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
        cmapSize = 2000, cmapNo = 20, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 10000, counterNo = 10
      )
      val sketch0 = AdaPerSketch.empty[Double]

      sketch0.deepUpdate(0.0 to 10.0 by 0.1: _*)
      ok
    }

    "rearrange" in {

      "basic" in {
        val cmapSize = 20
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          cmapSize = cmapSize, cmapNo = 2, cmapStart = Some(-10.0), cmapEnd = Some(10.0),
          queueSize = 50,
          counterSize = cmapSize
        )
        val sketch0 = AdaPerSketch.empty[Double]
        val (_, samples) = NumericDist.normal(0.0, 1).samples(100)

        val sketch1 = sketch0.narrowUpdate(samples: _*)
        val sketch2 = sketch1.rearrange
        val sketch3 = sketch2.narrowUpdate(samples: _*)

        ok
      }

    }

    "fastPdf" in {

      "queue only" in {
        val queueSize = 10
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 20, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]
        val p = 0.5

        val sketch1 = sketch0.update(0, 0, 1, 2, 4, 5)
        val interpPdf = Sketch.interpolationPdf(sketch1, p)
        val fastPdf = Sketch.fastPdf(sketch1, p)

        if(interpPdf ~= fastPdf) ok
        else ko(s"interpPdf: $interpPdf, fastPdf: $fastPdf")
      }

      "structure dominant" in {
        val queueSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 20, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]
        val p = 0.5

        val sketch1 = sketch0.update(0, 0, 1, 2, 4, 5)
        val interpPdf = Sketch.interpolationPdf(sketch1, p)
        val fastPdf = Sketch.fastPdf(sketch1, p)

        if(interpPdf ~= fastPdf) ok
        else ko(s"interpPdf: $interpPdf, fastPdf: $fastPdf")
      }

      "mixed" in {
        val queueSize = 3
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = queueSize,
          cmapSize = 20, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]
        val p = 0.5

        val sketch1 = sketch0.update(0, 1, 2, 4, 5, 0)
        val interpPdf = Sketch.interpolationPdf(sketch1, p)
        val fastPdf = Sketch.fastPdf(sketch1, p)

        if(interpPdf ~= fastPdf) ok
        else ko(s"interpPdf: $interpPdf, fastPdf: $fastPdf")
      }

      "multiple cmaps" in {
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          queueSize = 3,
          startThreshold = 2d, thresholdPeriod = 2d,
          cmapSize = 20, cmapNo = 3, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]
        val p = 2.2

        val sketch1 = sketch0.update(0, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5, 2, 2)
        val interpPdf = Sketch.interpolationPdf(sketch1, p)
        val fastPdf = Sketch.fastPdf(sketch1, p)

        if(interpPdf ~= fastPdf) ok
        else ko(s"interpPdf: $interpPdf, fastPdf: $fastPdf")
      }

    }

    // End

  }

}