package flip.sim

import flip.conf.pdf.SmoothDistConf
import flip.measure.syntax._
import flip.pdf.{Dist, PlottedDist}
import flip.sim.syntax._
import org.specs2.ScalaCheck
import org.specs2.mutable._

class L2SqSpec extends Specification with ScalaCheck {

  "L2Sq" should {

    "euclidean: parallel case" in {
      implicit val conf: SmoothDistConf = SmoothDistConf(samplingSize = 100)
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(0.0, 1)
      val expected = 0.0
      val error = 1E-2

      val euclidean = Euclidean(normal1, normal2)

      val cond1 = euclidean ~= (expected, error)

      if(!cond1) ko(s"Euclidean distance for the parallel case. calculated: $euclidean, expected: $expected. ")
      else ok
    }

    "euclidean: orthogonal case" in {
      implicit val conf: SmoothDistConf = SmoothDistConf(samplingSize = 20)
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(100.0, 1)
      val expected = math.sqrt(2)

      val euclidean = Euclidean(normal1, normal2)

      val cond1 = euclidean ~= expected

      if(!cond1) ko(s"Euclidean distance for the orthogonal case. calculated: $euclidean, expected: $expected.")
      else ok
    }

    "euclidean: diagonal case" in {
      implicit val conf: SmoothDistConf = SmoothDistConf(samplingSize = 100)
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(1.0, 1)
      val min = 0
      val max = math.sqrt(2)

      val euclidean = Euclidean(normal1, normal2)

      val cond1 = euclidean > min
      val cond2 = euclidean < max

      if(!cond1) ko(s"Euclidean distance for the orthogonal case. calculated $euclidean is smaller than $min")
      if(!cond2) ko(s"Euclidean distance for the orthogonal case. calculated: $euclidean is larger than $max")
      else ok
    }

  }

}