package flip.pdf.sampling

import flip.conf.EqualizeIcdfSamplingConf
import flip.pdf.NormalDist
import flip.measure.syntax._
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