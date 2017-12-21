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

    "countPloy" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )
      val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

      sketch0.countPlot.fold(ko("Fail to call the countPlot."))(plot => {
        val cond1 = plot.records.nonEmpty
        val cond2 = plot.records.forall { case (_, value) => !value.isNaN }
        if(cond1 && cond2) ok
        else if(!cond1) ko("Plot record is empty.")
        else if(!cond2) ko("Some value is NaN")
        else ko
      })
    }

    "densityPlot" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )
      val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

      sketch0.densityPlot.fold(ko("Fail to call the densityPlot."))(plot => {
        val cond1 = plot.records.nonEmpty
        val cond2 = plot.records.forall { case (_, value) => !value.isNaN }
        if(cond1 && cond2) ok
        else if(!cond1) ko("Plot record is empty.")
        else if(!cond2) ko("Some value is NaN")
        else ko
      })
    }

    "deepUpdate" in {

      "basic" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 3, -1, 10)
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)
        val sketch1O = sketch0.deepUpdate(1).map(_._1)

        println(s"cmap1: ${sketch0.lastCmap}, cmap2: ${sketch1O.flatMap(_.lastCmap)}")

        val cond1 = sketch0.lastCmap != sketch1O.flatMap(_.lastCmap)
        val cond2 = sketch0.lastCmap.map(_.size) == sketch1O.flatMap(_.lastCmap).map(_.size)

        if(cond1 && cond2) ok
        else ko(
          s"cmap1(${sketch0.lastCmap.map(_.size).getOrElse(0)}): ${sketch0.lastCmap}, " +
            s"cmap2(${sketch1O.flatMap(_.lastCmap).map(_.size).getOrElse(0)}): ${sketch1O.flatMap(_.lastCmap)}"
        )
      }

      "2 times" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, -1, 10)
        val (counterSize, counterNo) = (8, 2)
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

        println("2times: \n " + s"cmap0: $cmap0O, cmap1: $cmap1O, cmap2: $cmap2O")

        if(cmap0O != cmap1O && cmap1O != cmap2O) ok
        else ko(s"cmap0: $cmap0O, cmap1: $cmap1O, cmap2: $cmap2O")
      }

    }

    "count" in {

      "basic" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (8, 2)
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
        val (counterSize, counterNo) = (8, 2)
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
      val (counterSize, counterNo) = (8, 2)
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