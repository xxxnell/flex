package flip.pdf

import flip.conf._
import flip.implicits._
import org.specs2.ScalaCheck
import org.specs2.mutable._

class SketchMonadSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "Monad Ops" in {

      "map" in {

        "basic" in {
          val dataNo = 100
          val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 1, Some(0d), Some(10d))
          val (counterSize, counterNo) = (2, 1)
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
            counterSize = counterSize, counterNo = counterNo
          )
          val sketch0 = Sketch.empty[Double]
          val underlying = NumericDist.normal(0.0, 1)
          val (_, datas) = underlying.samples(dataNo)
          val sketch1 = sketch0.updateInOrder(datas)
          val sketch2 = sketch1.map(i => -1 * i)

          val cond = sketch2.sampling.records.forall { case (_, value) => !value.isNaN }

          if (!cond) ko(s"The result of map contains NaN: \n${sketch2.sampling}")
          else ok
        }

        "empty" in todo

//        "arbitrary" in {
//          (for {
//            sketch <- SketchGen.intSketchSample
//            utdSketch = sketch.map(i => -1 * i)
//          } yield utdSketch)
//            .fold(ko)(sketch => ok)
//        }

      }

      "flatMap" in {

        "basic" in {
          val dataNo = 100
          val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 1, Some(0d), Some(10d))
          val (counterSize, counterNo) = (2, 1)
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
            counterSize = counterSize, counterNo = counterNo
          )
          val sketch0 = Sketch.empty[Double]
          val underlying = NumericDist.logNormal(0.0, 1)
          val (_, datas) = underlying.samples(dataNo)
          val sketch1 = sketch0.updateInOrder(datas)
          val sketch2 = sketch1.flatMap(x => NumericDist.normal(x, 2))

          val cond = sketch2.sampling.records.forall { case (_, value) => !value.isNaN }

          if (!cond) ko(s"The result of flatMap contains NaN: \n${sketch2.sampling}")
          else ok
        }

        "empty" in todo

//        "arbitrary" in {
//          (for {
//            sketch <- SketchGen.intSketchSample
//            utdSketch = sketch.flatMap(i => NormalDist[Double](doubleMeasure, i, 1))
//          } yield utdSketch)
//            .fold(ko)(sketch => ok)
//        }

      }

      "for comprehension" in {

        "1-elem" in {
          val (_, samples) = NumericDist.normal(0.0, 1.0).samples(100)
          val sketch0 = Sketch.empty[Double].updateInOrder(samples)
          val sketch1 = for {
            x <- sketch0
          } yield math.exp(x)

          val cond1 = sketch1.sum > 0

          if(!cond1) ko("for comprehensed sketch is empty")
          else ok
        }

        "2-elem" in {
          val (_, samples) = NumericDist.normal(0.0, 1.0).samples(100)
          val sketch0 = Sketch.empty[Double].updateInOrder(samples)
          val sketch1 = for {
            x <- sketch0
            y <- NumericDist.normal(x, 1.0)
          } yield y

          val cond1 = sketch1.sum > 0

          if(!cond1) ko("for comprehensed sketch is empty")
          else ok
        }

      }

    }

  }

}