package flip.sim

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.conf.DistConf
import flip.pdf.{Dist, PlottedDist}
import flip.range.RangeP
import flip.measure.syntax._

class KLDSpec extends Specification with ScalaCheck {

  "KLD" should {

    "basic" in {
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(0.5, 1)
      val expected = 0.125

      val kld = KLD.simForDist(normal1, normal2)

      if(kld ~= expected) ok else ko(s"kld: $kld, expected: $expected")
    }

  }

}