package flex.chain.monad

import flex.implicits._
import flex.pdf.Normal
import flex.chain.Model
import flex.chain.Stream.syntax._
import org.specs2.mutable._
import org.specs2.ScalaCheck

class ModelSpec extends Specification with ScalaCheck {

  "Model" should {

    "flatMap" in {
      // μ ~ N(0, 1), σ ~ N(0, 1)
      val xsm = Model.stream(Normal(0, 1), Normal(0, 1))

      // y ~ N(μ, σ)
      def y(xs: Map[Int, Double]): Stream[Double] = {
        (for {
          mu <- xs.get(0)
          sigma <- xs.get(1)
        } yield Normal(mu, sigma)).fold(Model.sink)(y => Model.stream(y))
      }

      xsm.flatMap(xs => y(xs))
    }



  }

}