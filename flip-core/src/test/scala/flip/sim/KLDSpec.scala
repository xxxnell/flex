package flip.sim

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.conf.DistConf
import flip.pdf.Dist
import flip.range.RangeP
import flip.measure.syntax._

class KLDSpec extends Specification with ScalaCheck {

  "KLD" should {

    "basic" in {
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(0.5d, 1)
      implicit val conf: DistConf = DistConf(1e-5)
      val samples = (-2.5 to 2.5 by 0.1).toList.sliding(2).flatMap {
        case s1 :: s2 :: Nil => Some(RangeP(s1, s2))
        case _ => None
      }.toList
      val expected = 0.1234

      (for {
        sampling <- normal1.sampling(samples)
        kld <- KLD(sampling, normal2)
      } yield kld)
        .fold(ko("Error occurs.")){ kld =>
          if(kld ~= expected) ok else ko(s"kld: $kld, expected: $expected")
        }
    }

  }

}