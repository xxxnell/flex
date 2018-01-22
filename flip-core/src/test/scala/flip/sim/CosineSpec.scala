package flip.sim

import flip.conf.DistConf
import flip.pdf.Dist
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.measure.syntax._

class CosineSpec extends Specification with ScalaCheck {

  "Cosine" should {

    "basic" in {
      val normal1 = Dist.normal(0.0, 1)
      val normal2 = Dist.normal(0.0, 1)
      val conf: DistConf = DistConf(1e-5)
      val expect = 1.0

      (for {
        sampling <- normal1.uniformSampling(-3.0, 3.0, 100)
        cosine <- Cosine.simForPlotted(sampling, normal2, conf)
        cosineDensity <- Cosine.simDensityForPlotted(sampling, normal2, conf)
        _ = println(s"sampling: \n${sampling.sampling.dsv("\t")}")
        _ = println(s"cosineDensity: \n${cosineDensity.dsv("\t")}")
      } yield cosine)
        .fold(ko("Exception occurs."))(cosine =>
          if(cosine ~= expect) ok else ko(s"Cosine similarity $cosine is not $expect. ")
        )
    }

  }

}