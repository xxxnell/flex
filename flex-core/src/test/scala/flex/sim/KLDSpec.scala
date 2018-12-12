package flex.sim

import flex.conf.pdf.SmoothDistConf
import flex.measure.syntax._
import flex.pdf.Dist
import org.specs2.ScalaCheck
import org.specs2.mutable._

class KLDSpec extends Specification with ScalaCheck {

  "KLD" should {

    "basic" in {
      implicit val conf: SmoothDistConf = SmoothDistConf(samplingSize = 500, samplingBoundaryRatio = 0.01)
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(0.5, 1)
      val expected = 0.125

      val kld = KLD.simForDist(normal1, normal2)
      val cond1 = kld ~= expected

      if(!cond1) ko(s"kld: $kld, expected: $expected") else ok
    }

  }

}