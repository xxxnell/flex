package flip.sim

import flip.measure.syntax._
import flip.pdf.{Dist, PlottedDist}
import flip.sim.syntax._
import org.specs2.ScalaCheck
import org.specs2.mutable._

class L2SqSpec extends Specification with ScalaCheck {

  "L2Sq" should {

    "euclidean basic 1" in {
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(0.0, 1)
      val expect = 0.0
      val error = 5E-2

      val euclidean = Euclidean(normal1, normal2)

      val cond1 = euclidean ~= (expect, error)

      if(!cond1) ko(s"Euclidean distance $euclidean is not $expect. ")
      else ok
    }

    "euclidean basic 2" in {
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(10.0, 1)
      val lowerBound = 1.0

      val euclidean = Euclidean(normal1, normal2)

      val cond1 = euclidean > lowerBound

      if(!cond1) ko(s"Euclidean distance $euclidean is smaller then $lowerBound.")
      else ok
    }

  }

}