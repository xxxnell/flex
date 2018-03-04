package flip.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.conf._
import flip.measure._
import flip.pdf.syntax._
import cats.implicits._
import flip.cmap.{Cmap, DividerCmap}

class PeriodicSketchSpec extends Specification with ScalaCheck {

  "PeriodicSketch" should {

    "empty & cmapSize > counterSize" in {
      // construct
      val (cmapSize, cmapNo, cmapStart, cmapEnd) = (50, 100, Some(-100d), Some(100d))
      val (counterSize, counterNo) = (10, 2)
      implicit val conf: PeriodicSketchConf = CustomSketchConf(
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
      implicit val conf: PeriodicSketchConf = CustomSketchConf(
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

    "periods" in {
      (for {
        periodicSketch <- PeriodicSketchGen.periodicSketchSample
      } yield periodicSketch)
        .fold(ko)(periodicSketch => ok)
    }

    "update" in {

      "check cmap changes for periodic sketch" in {
        val (start, period) = (0, 1)
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 1, Some(0d), Some(10d))
        val (counterSize, counterNo) = (2, 1)
        implicit val conf: PeriodicSketchConf = CustomSketchConf(
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

  }

}

object PeriodicSketchGen {

  def intPeriodicSketchGen: Gen[PeriodicSketch[Int]] = for {
    measure <- MeasureGen.intMeasureGen
    conf <- SketchConfGen.sketchConfGen
  } yield PeriodicSketch.empty(measure, conf)

  def periodicSketchSample: Option[PeriodicSketch[Int]] = intPeriodicSketchGen.sample

}