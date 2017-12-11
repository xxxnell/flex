package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.conf.{SketchConf, SketchConfGen}
import sketch.scope.measure._

/**
  * Licensed by Probe Technology, Inc.
  */
class ContSketchSpec extends Specification with ScalaCheck {

  "ContSketch" should {

    "empty" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax, counterSize, counterNo) =
        (5, 100, Double.MinValue, Double.MaxValue, 10, 2)
      implicit val conf: SketchConf = SketchConf(cmapSize, cmapNo, cmapMin, cmapMax, counterSize, counterNo)
      val contSketch = ContSketch.empty[Int]

      val testCaDepth = contSketch.structures.lengthCompare(cmapNo) == 0

      val testCaSize = contSketch
        .structures
        .map { case (cmap, hcounter) => cmap.size - 1 }
        .forall(_ == cmapSize)

      val testCoDepth = contSketch
        .structures
        .map { case (cmap, hcounter) => hcounter.depth }
        .forall(_ == counterNo)

      val testCoSize = contSketch
        .structures
        .map { case (cmap, hcounter) => hcounter.width }
        .forall(_ == counterSize)

      if(testCaDepth &&
        testCaSize &&
        testCoDepth &&
        testCoSize) ok
      else ko(
        s"testCaDepth: $testCaDepth, " +
          s"testCaSize: $testCaSize, " +
          s"testCoDepth: $testCoDepth, " +
          s"testCoSize: $testCoSize"
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

  def intContSketchGen: Gen[ContSketch[Int]] = for {
    measure <- MeasureGen.intMeasureGen
    conf <- SketchConfGen.sketchConfGen
  } yield ContSketch.empty(measure, conf)

  def contSketchSample: Option[ContSketch[Int]] = intContSketchGen.sample

}