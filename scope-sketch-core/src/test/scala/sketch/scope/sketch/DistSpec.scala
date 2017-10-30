package sketch.scope.sketch

import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class DistSpec extends Specification with ScalaCheck {

  "Dist" should {

    "monad ops" in {

      "map" in {
        Dist.delta[Double].map(x => x + 10).probability(10, 10).fold(ko)(prob => if(prob > 0) ok else ko)
      }

      "flatMap" in {

        "basic" in {
          val res: Dist[Double] = Dist.delta[Double].flatMap(_ => Dist.delta[Double])
          res must beAnInstanceOf[Dist[Double]]
        }

        "with sketch" in {
          val res: Sketch[Double] = Dist.delta[Double].flatMap(_ => Sketch.empty[Double](identity, 1, 1, 1, 1))
          res must beAnInstanceOf[Sketch[Double]]
        }

      }

      "for comprehension" in {

        "basic" in {
          val res = for {
            x <- Dist.delta[Double]
            y <- Dist.delta[Double]
          } yield x + y
          res must beAnInstanceOf[Dist[Double]]
        }

        "with sketch" in {
          val res = for {
            x <- Dist.delta[Double]
            y <- Sketch.empty[Double](identity, 1, 1, 1, 1)
          } yield x + y
          res must beAnInstanceOf[Sketch[Double]]
        }

      }

    }

  }

}