package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable._
import sketch.scope.conf._
import sketch.scope.measure._

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchPropSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "update" in {

      "update int" in {
        (for {
          sketch <- SketchGen.intSketchSample
          updated <- sketch.update(1)
        } yield updated)
          .fold(ko)(sketch => ok)
      }

      "update boolean" in {
        (for {
          sketch <- SketchGen.booleanSketchSample
          updated <- sketch.update(true)
        } yield updated)
          .fold(ko)(sketch => ok)
      }

    }

    "deepUpdate" in {

      "basic" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (2, 1)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)
        val sketch1O = sketch0.deepUpdate(1).map(_._1)

        if(sketch0.lastCmap != sketch1O.flatMap(_.lastCmap)) ok
        else ko(s"cmap1: ${sketch0.lastCmap}, cmap2: ${sketch1O.flatMap(_.lastCmap)}")
      }

      "2 times" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (2, 1)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)
        val sketch1O = sketch0.deepUpdate(1).map(_._1)
        val sketch2O = sketch1O.flatMap(sketch1 => sketch1.deepUpdate(1).map(_._1))

        val cmap0O = sketch0.lastCmap
        val cmap1O = sketch1O.flatMap(_.lastCmap)
        val cmap2O = sketch2O.flatMap(_.lastCmap)

        if(cmap0O != cmap1O && cmap1O != cmap2O) ok
        else ko(s"cmap0: $cmap0O, cmap1: $cmap1O, cmap2: $cmap2O")
      }

    }

    "count" in {

      "basic" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (2, 1)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        (for {
          sketch1 <- sketch0.update(1)
          count <- sketch1.count(0, 10)
        } yield count)
          .fold(ko)(count => if(count > 0) ok else ko(s"count: $count"))
      }

      "empty" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (2, 1)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        (for {
          count <- sketch0.count(0, 10)
        } yield count)
          .fold(ko)(count => if(count == 0) ok else ko(s"count: $count"))
      }

    }

    "sum" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
      val (counterSize, counterNo) = (2, 1)
      implicit val conf: SketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )
      val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

      (for {
        sketch1 <- sketch0.update(1)
        sum = sketch1.sum
      } yield sum)
        .fold(ko)(sum => if(sum > 0 && sum <= 1) ok else ko(s"sum: $sum"))
    }

  }

}

object SketchGen {

  def intSketchGen: Gen[Sketch[Int]] = sketchGen[Int]

  def booleanSketchGen: Gen[Sketch[Boolean]] = sketchGen[Boolean]

  def sketchGen[A](implicit measure: Measure[A]): Gen[Sketch[A]] = for {
    conf <- SketchConfGen.sketchConfGen
  } yield Sketch.empty[A](measure, conf)

  def intSketchSample: Option[Sketch[Int]] = intSketchGen.sample

  def booleanSketchSample: Option[Sketch[Boolean]] = booleanSketchGen.sample

}