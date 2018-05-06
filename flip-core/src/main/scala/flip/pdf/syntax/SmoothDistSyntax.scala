package flip.pdf.syntax

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.pdf.{PlottedDist, SmoothDist}
import flip.range.syntax.RangeP

trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {
    def samplingDist(implicit measure: Measure[A], conf: SamplingDistConf): PlottedDist[A] =
      PlottedDist.pointPlot[A](dist.sampling)
  }

//  implicit class PlottedDistPropSyntaxImpl[A](dist: PlottedDist[A]) {
//    def filter(f: RangeP => Boolean): PlottedDist[A] = PlottedDist.filter(dist, f)
//  }

}
