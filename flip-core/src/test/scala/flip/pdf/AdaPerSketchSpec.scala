package flip.pdf

import flip._
import flip.conf.pdf.{CustomAdaPerSketchConf, CustomPeriodicSketchConf, PeriodicSketchConf}
import flip.measure.syntax._
import flip.pdf.Buffer.syntax._
import org.specs2.ScalaCheck
import org.specs2.mutable._

import scala.language.postfixOps

class AdaPerSketchSpec extends Specification with ScalaCheck {

  "AdaPerSketch" should {

    "construct" in {

      "empty & cmapSize > counterSize" in {
        // construct
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (50, 100, Some(-100d), Some(100d))
        val (counterSize, counterNo) = (10, 2)
        implicit val conf: PeriodicSketchConf = CustomAdaPerSketchConf(
          startThreshold = 0, thresholdPeriod = 1,
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val periodicSketch = PeriodicSketch.empty[Int]

        // test
        val strSize = periodicSketch.structures.size
        val cmapSizes = periodicSketch.
          structures
          .map { case (cmap, hcounter) => cmap.size }
        val counterNos = periodicSketch
          .structures
          .map { case (cmap, hcounter) => hcounter.depth }
        val counterSizes = periodicSketch
          .structures
          .map { case (cmap, hcounter) => hcounter.width }

        val cond1 = strSize == 1
        val cond2 = cmapSizes.forall(_ == cmapSize)
        val cond3 = counterNos.forall(_ == counterNo)
        val cond4 = counterSizes.forall(_ == counterSize)

        if(!cond1) ko(s"strSize: $strSize (expected: 1)")
        else if(!cond2) ko(s"cmapSizes: $cmapSizes (expected: $cmapSize)")
        else if(!cond3) ko(s"counterNos: $counterNos (expected: $counterNo)")
        else if(!cond4) ko(s"counterSizes: $counterSizes (expected: $counterSize)")
        else ok
      }

      "empty & cmapSize == counterSize" in {
        // construct
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 100, Some(-100d), Some(100d))
        val (counterSize, counterNo) = (10, 2)
        implicit val conf: PeriodicSketchConf = CustomAdaPerSketchConf(
          startThreshold = 0, thresholdPeriod = 1,
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val periodicSketch = PeriodicSketch.empty[Int]

        // test
        val strSize = periodicSketch.structures.size
        val cmapSizes = periodicSketch.
          structures
          .map { case (cmap, hcounter) => cmap.size }
        val counterNos = periodicSketch
          .structures
          .map { case (cmap, hcounter) => hcounter.depth }
        val counterSizes = periodicSketch
          .structures
          .map { case (cmap, hcounter) => hcounter.width }

        val cond1 = strSize == 1
        val cond2 = cmapSizes.forall(_ == cmapSize)
        val cond3 = counterNos.forall(_ == 1)
        val cond4 = counterSizes.forall(_ == counterSize)

        if(!cond1) ko(s"strSize: $strSize (expected: 1)")
        else if(!cond2) ko(s"cmapSizes: $cmapSizes (expected: $cmapSize)")
        else if(!cond3) ko(s"counterNos: $counterNos (expected: $counterNo)")
        else if(!cond4) ko(s"counterSizes: $counterSizes (expected: $counterSize)")
        else ok
      }

    }

    "update" in {

      "check cmap changes for periodic sketch" in {
        val (start, period) = (0, 1)
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 1, Some(0d), Some(10d))
        val (counterSize, counterNo) = (2, 1)
        implicit val conf: PeriodicSketchConf = CustomAdaPerSketchConf(
          startThreshold = start, thresholdPeriod = period,
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val sketch0: Sketch[Double] = PeriodicSketch.empty[Double]
        val datas = Stream.from(1, 1).take(3).toList

        var sketch = sketch0
        val seqSketches = datas.map { data => sketch = sketch.update(data); sketch }
        val seqCmaps = seqSketches.map(sketch => sketch.youngCmap)

        val cond = seqCmaps.sliding(2).forall {
          case cmap1 :: cmap2 :: Nil => cmap1 != cmap2
          case _ => false
        }

        if(cond) ok else ko(
          seqCmaps.zipWithIndex.map { case (cmap, idx) => s"cmap ${idx+1}: $cmap"}.mkString("\n")
        )
      }

    }

    "type invariance" in {

      "type invariance after update" in {
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = 10,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0: Sketch[Double] = AdaPerSketch.empty[Double]
        val sketch1 = sketch0.update(1)

        if(!sketch1.isInstanceOf[AdaPerSketch[Double]]) ko
        else if(sketch1.asInstanceOf[AdaPerSketch[Double]].buffer.isEmpty) ko("Buffer is not cleaned.")
        else ok
      }

      "type invariance after rebuild" in {
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = 10,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch = AdaPerSketch.empty[Double]
        val sketch1 = sketch.update(1)
        val sketch2 = sketch1.rebuild

        if(!sketch2.isInstanceOf[AdaPerSketch[Double]]) ko
        else if(sketch2.asInstanceOf[AdaPerSketch[Double]].buffer.nonEmpty) ko("Buffer is not cleaned.")
        else ok
      }

    }

    "buffer" in {

      "empty buffer" in {
        val bufferSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch = AdaPerSketch.empty[Double]
        val buffer = sketch.asInstanceOf[AdaPerSketch[Double]].buffer

        if(buffer.nonEmpty) ko("Buffer of empty sketch have to be empty.") else ok
      }

      "buffer io" in {
        val bufferSize = 2
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch = AdaPerSketch.empty[Double]

        val sketch1 = sketch.update(1)
        val sketch2 = sketch1.update(2)
        val sketch3 = sketch2.update(3)
        val q2 = sketch2.asInstanceOf[AdaPerSketch[Double]].buffer
        val q3 = sketch3.asInstanceOf[AdaPerSketch[Double]].buffer

        if(q3.size != bufferSize) ko(s"buffer1 size: ${q3.size}.")
        else if(q3.size != q3.size) ko(s"buffer1 size(${q3.size}) and buffer2 size(${q3.size}) have to equal.")
        else if(q2.headOption == q3.headOption) ko(s"buffer1: $q2 == buffer2: $q3")
        else ok
      }

    }

    "count" in {

      "buffer only" in {
        val bufferSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val count = sketch1.count(1, 1)

        if(count <= 0) ko(s"Counts nothing: $count") else ok
      }

      "buffer and structure" in {
        val bufferSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
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

      "buffer only" in {
        val bufferSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
          cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val sum = sketch1.sum

        if(sum < 1) ko(s"Counts nothing: $sum") else ok
      }

      "buffer and structure" in {
        val bufferSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
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

    "rebuild" in {

      "basic" in {
        val cmapSize = 20
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          cmapSize = cmapSize, cmapNo = 2, cmapStart = Some(-10.0), cmapEnd = Some(10.0),
          bufferSize = 50,
          counterSize = cmapSize
        )
        val sketch0 = AdaPerSketch.empty[Double]
        val (_, samples) = NumericDist.normal(0.0, 1).samples(100)

        val sketch1 = sketch0.narrowUpdate(samples: _*)
        val sketch2 = sketch1.rebuild
        val sketch3 = sketch2.narrowUpdate(samples: _*)

        ok
      }

    }

    "fastPdf" in {

      "buffer only" in {
        val bufferSize = 10
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
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
        val bufferSize = 1
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
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
        val bufferSize = 3
        implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
          bufferSize = bufferSize,
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
          bufferSize = 3,
          startThreshold = 2d, thresholdPeriod = 2d,
          cmapSize = 20, cmapNo = 3, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 20, counterNo = 2
        )
        val sketch0 = AdaPerSketch.empty[Double]
        val p = 2.2

        val sketch1 = sketch0.update(0, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5, 2, 2)
        val interpPdf = Sketch.interpolationPdf(sketch1, p)
        val fastPdf = Sketch.fastPdf(sketch1, p)

        if(interpPdf.~=(fastPdf)(0.15)) ok // TODO There is some error between interpPdf and fastPdf (#46)
        else ko(s"interpPdf: $interpPdf, fastPdf: $fastPdf")
      }

    }

    // End

  }

}