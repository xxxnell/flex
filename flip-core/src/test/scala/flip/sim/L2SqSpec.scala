package flip.sim

import flip.measure.syntax._
import flip.pdf.Dist
import flip.sim.syntax._
import org.specs2.ScalaCheck
import org.specs2.mutable._

class L2SqSpec extends Specification with ScalaCheck {

  "L2Sq" should {

    "euclidean basic" in {
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(0.0, 1)
      val expect = 0.0
      val error = 1E-3

      (for {
        sampling <- normal1.uniformSampling(-3.0, 3.0, 100)
        euclidean <- Euclidean(sampling, normal2)
      } yield euclidean)
        .fold(ko("Exception occurs."))(euclidean => {
          val cond1 = euclidean ~= (expect, error)

          if(!cond1) ko(s"Cosine similarity $euclidean is not $expect. ")
          else ok
        })
    }

  }

}