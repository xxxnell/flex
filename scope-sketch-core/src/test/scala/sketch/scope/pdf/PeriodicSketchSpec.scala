package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.conf.{CmapConf, CounterConf, SketchConf, SketchConfGen}
import sketch.scope.measure._

/**
  * Licensed by Probe Technology, Inc.
  */
class PeriodicSketchSpec extends Specification with ScalaCheck {

  "ContSketch" should {

    "empty" in {
      // construct
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (5, 100, -100, 100)
      val (counterSize, counterNo) = (10, 2)
      implicit val conf: SketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, Some(cmapMin), Some(cmapMax)),
        CounterConf(counterSize, counterNo)
      )
      val contSketch = PeriodicSketch.empty[Int]

      // test
      val strSize = contSketch.structures.size
      val cmapSizes = contSketch
        .structures
        .map { case (cmap, hcounter) => cmap.size }
      val counterNos = contSketch
        .structures
        .map { case (cmap, hcounter) => hcounter.depth }
      val counterSizes = contSketch
        .structures
        .map { case (cmap, hcounter) => hcounter.width }

      if(strSize == cmapNo &&
        cmapSizes.forall(_ == cmapSize) &&
        counterNos.forall(_ == counterNo) &&
        counterSizes.forall(_ == counterSize)) ok
      else ko(
        s"strSize: $strSize (expected: $cmapNo), " +
          s"cmapSizes: $cmapSizes (expected: $cmapSize), " +
          s"counterNos: $counterNos (expected: $counterNo), " +
          s"counterSizes: $counterSizes (expected: $counterSize)"
      )
    }

    "periods" in {
      (for {
        contSketch <- ContSketchGen.contSketchSample
      } yield contSketch)
        .fold(ko)(contsketch => ok)
    }

  }

}

object ContSketchGen {

  def intContSketchGen: Gen[PeriodicSketch[Int]] = for {
    measure <- MeasureGen.intMeasureGen
    conf <- SketchConfGen.sketchConfGen
  } yield PeriodicSketch.empty(measure, conf)

  def contSketchSample: Option[PeriodicSketch[Int]] = intContSketchGen.sample

}