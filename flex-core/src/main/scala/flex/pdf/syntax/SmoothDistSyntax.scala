package flex.pdf.syntax

import flex.conf.pdf.SamplingDistConf
import flex.measure.Measure
import flex.pdf.{PlottedDist, SmoothDist}

trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {
    def samplingDist(implicit measure: Measure[A], conf: SamplingDistConf): PlottedDist[A] =
      PlottedDist.forPdfSampling[A](dist.pdfSampling)
  }

//  implicit class PlottedDistPropSyntaxImpl[A](dist: PlottedDist[A]) {
//    def filter(f: RangeP => Boolean): PlottedDist[A] = PlottedDist.filter(dist, f)
//  }

}
