package sketch.scope.sim

import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.pdf.Dist
import sketch.scope.range.RangeP

/**
  * Licensed by Probe Technology, Inc.
  */
class KLDSpec extends Specification with ScalaCheck {

  "KLD" should {

    "basic" in {
      val normal = Dist.normal(0d, 1)
      val samples = (-2.5 to 2.5 by 0.01).toList.sliding(2).map { case s1 :: s2 :: Nil => RangeP(s1, s2) }.toList

      (for {
        sampling <- normal.toSampleDist(samples)
        kld <- KLD(sampling, normal)
        _ = println(kld)
      } yield kld)
        .fold(ko("Error occurs.")){ kld =>
          if(kld > 0) ok else ko(s"kld: $kld") }
    }

  }

}