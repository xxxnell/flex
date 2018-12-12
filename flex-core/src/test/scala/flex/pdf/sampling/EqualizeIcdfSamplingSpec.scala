package flex.pdf.sampling

import flex.conf.pdf.EqualizeIcdfSamplingConf
import flex.pdf.NormalDist
import flex.measure.syntax._
import org.specs2.mutable._
import org.specs2.ScalaCheck

class EqualizeIcdfSamplingSpec extends Specification with ScalaCheck {

  "EqualizeIcdfSampling" should {

    "basic" in {
      val size = 10
      val icdf = NormalDist.apply(1.0, 1.0).icdf(_)
      val measure = doubleMeasure
      val conf = EqualizeIcdfSamplingConf(size, 1)
      val sampling = EqualizeIcdfSampling.sampling(icdf, measure, conf)

      val cond1 = sampling.size == size - 1 // TODO See #44

      if(!cond1) ko(s"sampling size: ${sampling.size}, expected: $size")
      else ok
    }

  }

}