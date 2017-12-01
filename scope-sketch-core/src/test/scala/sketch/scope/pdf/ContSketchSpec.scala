package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class ContSketchSpec extends Specification with ScalaCheck {

  "ContSketch" should {

    "empty" in {
      val measure: Int => Double = (a: Int) => a.toDouble
      val caDepth = 10
      val caSize = 20
      val coDepth = 30
      val coSize = 40

      val contSketch = ContSketch.empty(measure, caDepth, caSize, coDepth, coSize)

      val testMeasure = contSketch.measure == measure

      val testCaDepth = contSketch.structure.size == caDepth

      val testCaSize = contSketch
        .structure
        .map { case (cmap, hcounter) => cmap.size -1 == caSize }
        .forall(b => b)

      val testCoDepth = contSketch
        .structure
        .map { case (cmap, hcounter) => hcounter.depth == coDepth }
        .forall(identity)

      val testCoSize = contSketch
        .structure
        .map { case (cmap, hcounter) => hcounter.width == coSize }
        .forall(identity)

      if(testMeasure &&
        testCaDepth &&
        testCaSize &&
        testCoDepth &&
        testCoSize) ok
      else ko(
        s"testMeasure: $testMeasure, " +
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
    caDepth <- Gen.choose(1, 10)
    caSize <- Gen.choose(100, 10000)
    coDepth <- Gen.choose(1, 10)
    coSize <- Gen.choose(100, 10000)
  } yield ContSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def contSketchSample: Option[ContSketch[Int]] = intContSketchGen.sample

}