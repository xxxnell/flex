package flex.pdf

import flex.implicits._
import org.specs2.ScalaCheck
import org.specs2.mutable._

class DistSpec extends Specification with ScalaCheck {

  "Dist" should {

    "prop ops" in {

      "pdfSampling" in {

        "NumericDist" in {
          val sampling = NumericDist.normal(0.0, 1.0).pdfSampling
          val cond = sampling.records.map(_._2).forall(value => !value.isNaN && !value.isInfinity)

          if(!cond) ko
          else ok
        }

        "CombinationDist" in {
          val sampling = ((0.5, NumericDist.normal(-2.0, 1)) + (0.5, NumericDist.normal(2.0, 1))).pdfSampling
          val cond = sampling.records.map(_._2).forall(value => !value.isNaN && !value.isInfinity)

          if(!cond) ko
          else ok
        }

        "Sketch" in {
          val sampling = Sketch.empty[Double].pdfSampling
          val cond = sampling.records.map(_._2).forall(value => !value.isNaN && !value.isInfinity)

          if(!cond) ko
          else ok
        }

      }

    }

    "monad ops" in {

      "map" in {
//        Dist.delta[Double].map(x => x + 10).probability(10, 10).fold(ko)(prob => if(prob > 0) ok else ko)

        todo
      }

      "flatMap" in {

        "basic" in {
//          val res: Dist[Double] = Dist.delta[Double].flatMap(_ => Dist.delta[Double])
//          res must beAnInstanceOf[Dist[Double]]

          todo
        }

        "with sketch" in {
//          val res: Sketch[Double] = Dist.delta[Double].flatMap(_ => Sketch.empty[Double])
//          res must beAnInstanceOf[Sketch[Double]]

          todo
        }

      }

      "for comprehension" in {

        "basic" in {
//          val res = for {
//            x <- Dist.delta[Double]
//            y <- Dist.delta[Double]
//          } yield x + y
//          res must beAnInstanceOf[Dist[Double]]

          todo
        }

        "with sketch" in {
//          val res = for {
//            x <- Dist.delta[Double]
//            y <- Sketch.empty[Double]
//          } yield x + y
//          res must beAnInstanceOf[Sketch[Double]]

          todo
        }

      }

    }

  }

}