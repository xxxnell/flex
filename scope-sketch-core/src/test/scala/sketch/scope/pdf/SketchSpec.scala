package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable._
import sketch.scope.conf._
import sketch.scope.measure._

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "Prop ops" in {

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
          val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 1, 0, 10)
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
          val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 1, 0, 10)
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

      "get" in {
        (for {
          sketch <- SketchGen.intSketchSample
          updated <- sketch.update(1)
          res <- sketch.count(0, 10)
        } yield res)
          .fold(ko)(sketch => ok)
      }

    }

    "Monad Ops" in {

      "map" in {
        (for {
          sketch <- SketchGen.intSketchSample
          utdSketch = sketch.map(i => -1 * i)
        } yield utdSketch)
          .fold(ko)(sketch => ok)
      }

      "flatMap" in {
        (for {
          sketch <- SketchGen.intSketchSample
          utdSketch = sketch.flatMap(i => NormalDist[Double](doubleMeasure, i, 1))
        } yield utdSketch)
          .fold(ko)(sketch => ok)
      }

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