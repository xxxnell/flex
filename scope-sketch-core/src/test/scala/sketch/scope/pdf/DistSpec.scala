package sketch.scope.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.measure._
import sketch.scope.conf._

/**
  * Licensed by Probe Technology, Inc.
  */
class DistSpec extends Specification with ScalaCheck {

  "Dist" should {

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