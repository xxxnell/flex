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

    "count" in {

      "basic 1" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        (for {
          sketch1 <- sketch0.update(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
          count <- sketch1.count(1, 5)
        } yield count)
          .fold(ko)(count => if(count < 10) ok else ko(s"count: $count, expected: 5"))
      }

      "basic 2" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        (for {
          sketch1 <- sketch0.update(1, 2, 3, 4, 5, 6, 7, 8, 9)
          count <- sketch1.count(0, 10)
        } yield count)
          .fold(ko)(count => if(count == 9) ok else ko(s"count: $count, expected: 10"))
      }

      "count smaller space then cmap bound" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        (for {
          sketch1 <- sketch0.update(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
          count <- sketch1.count(1.1, 1.2)
        } yield count)
          .fold(ko)(count => if(count < 1) ok else ko(s"count: $count, expected: <1"))
      }

    }

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

    "narrowUpdate" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, -10, 10)
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )
      val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

      (for {
        sketch <- sketch0.narrowUpdate(0)
        count <- sketch.count(-1, 1)
      } yield count)
        .fold(ko("Exception occurs."))(count => if(count > 0) ok else ko(s"count: $count, expected: 0<x<1"))
    }

    "probability" in {

      "empty" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, -10, 10)
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        sketch0.probability(0, 1)
          .fold(ko)(prob => if(prob > 0) ok else ko(s"probability: $prob"))
      }

      "from min to 0 after updated" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        (for {
          sketch1 <- sketch0.update(-1)
          prob1 <- sketch1.probability(Double.MinValue, 0)
          prob2 <- sketch1.probability(0, Double.MaxValue)
        } yield (prob1, prob2))
          .fold(ko){ case (prob1, prob2) =>
            val cond1 = prob1 ~= 1d
            val cond2 = prob2 ~= 0d

            if(cond1 && cond2) ok
            else ko(s"probability for [-∞, 0]: $prob1, probability for [0, +∞]: $prob2")
          }
      }

    }

    "countPlot" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, -10, 10)
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )
      val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

      sketch0.countPlot.fold(ko("Fail to call the countPlot."))(plot => {
        val cond1 = plot.records.nonEmpty
        val cond2 = plot.records.forall { case (_, value) => !value.isNaN }
        val cond3 = plot.records.headOption.forall { case (range, _) => range.end ~= cmapMin }
        val cond4 = plot.records.lastOption.forall { case (range, _) => range.start ~= cmapMax }

        if(!cond1) ko("Plot record is empty.")
        else if(!cond2) ko("Some value is NaN")
        else if(!cond3) ko(s"${plot.records.headOption} is first range. cmapMin: $cmapMin")
        else if(!cond4) ko(s"${plot.records.lastOption} is last range. cmapMax: $cmapMax")
        else ok
      })
    }

    "densityPlot" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, -10, 10)
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )
      val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

      sketch0.densityPlot.fold(ko("Fail to call the densityPlot."))(plot => {
        val cond1 = plot.records.nonEmpty
        val cond2 = plot.records.forall { case (_, value) => !value.isNaN }
        val cond3 = plot.records.headOption.forall { case (range, _) => range.end ~= cmapMin }
        val cond4 = plot.records.lastOption.forall { case (range, _) => range.start ~= cmapMax }

        if(!cond1) ko("Plot record is empty.")
        else if(!cond2) ko("Some value is NaN")
        else if(!cond3) ko(s"${plot.records.headOption} is first range. cmapMin: $cmapMin")
        else if(!cond4) ko(s"${plot.records.lastOption} is last range. cmapMax: $cmapMax")
        else ok
      })
    }

    "deepUpdate" in {

      "changing cmap" in {

        "basic" in {
          val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 3, -1, 10)
          val (counterSize, counterNo) = (8, 2)
          implicit val conf: SketchConf = SketchConf(
            CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
            CounterConf(counterSize, counterNo)
          )
          val sketch0 = Sketch.empty[Double](doubleMeasure, conf)
          val sketch1O = sketch0.deepUpdate(1).map(_._1)

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

          if(cmap0O != cmap1O && cmap1O != cmap2O) ok
          else ko(s"cmap0: $cmap0O, cmap1: $cmap1O, cmap2: $cmap2O")
        }

      }

      "migrate countings" in {
//        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 3, 0, 10)
//        val (counterSize, counterNo) = (8, 2)
//        implicit val conf: SketchConf = SketchConf(
//          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
//          CounterConf(counterSize, counterNo)
//        )
//        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)
//        val sketch1O = sketch0.narrowU(5.5).map(_._1)

        todo
      }

    }

    "rearrange" in {
//      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 3, 0, 10)
//      val (counterSize, counterNo) = (8, 2)
//      val conf: SketchConf = SketchConf(
//        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
//        CounterConf(counterSize, counterNo)
//      )
//      val sketch0 = Sketch.empty[Double](doubleMeasure, conf)
//
//      (for {
//        sketch1 <- sketch0.narrowUpdate(5.5)
//        sketch2 <- sketch1.rearrange
//        count1 <- sketch2.count(5, 6)
//        count2 <- sketch2.count(0, 1)
//      } yield (count1, count2))
//        .fold(ko){ case (count1, count2) =>
//          val count1Exp = 1d - 1d / (cmapSize.toDouble - 2)
//          val cond1 = count1 ~= count1Exp
//          val cond2 = count2 ~= 0
//
//          if(cond1 && cond2) ok
//          else ko(
//            s"count 5<x<6: $count1 (expected: $count1Exp), " +
//              s"count 0<x<1: $count2 (expected: 0)"
//          )
//        }

      todo
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
          sum = sketch1.sum
        } yield sum)
          .fold(ko)(sum => if(sum > 0 && sum <= 1) ok else ko(s"sum: $sum"))
      }

      "after updated" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

        (for {
          sketch1 <- sketch0.update(1, 2, 3, 4, 5)
          sum = sketch1.sum
        } yield sum)
          .fold(ko)(sum => if(sum ~= 5) ok else ko(s"sum: $sum, expected: 5"))
      }

    }

    /** End **/

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