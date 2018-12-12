package flex.pdf.syntax

import flex.cmap.Cmap
import flex.conf.pdf.SketchConf
import flex.measure.Measure
import flex.pdf.monad.{SketchBind, SketchFunctor}
import flex.pdf.{Count, Dist, Histogram, Sketch}
import flex.plot.{DensityPlot, PointPlot}
import flex.range.RangeM

trait SketchSyntax extends SketchPropSyntax

trait SketchPropSyntax {

  implicit class SketchPropSyntaxImpl[A](sketch: Sketch[A]) {
    def samplingPoints: List[RangeM[A]] = Sketch.samplingPoints(sketch)
    def sample: (Sketch[A], A) = Sketch.sample(sketch)
    def samples(n: Int): (Sketch[A], List[A]) = Sketch.samples(sketch, n)
    def update(as: A*): Sketch[A] =
      Sketch.update(sketch, as.toList.map(a => (a, 1d)))
    def update(as: List[(A, Count)]): Sketch[A] =
      Sketch.update(sketch, as)
    def updateInOrder(as: List[A]): Sketch[A] =
      as.foldLeft(sketch) { case (_sketch, a) => Sketch.update(_sketch, (a, 1d) :: Nil) }
    def updateTrace(as: List[A]): List[Sketch[A]] = {
      var temp: Sketch[A] = sketch
      as.map { a =>
        temp = temp.update(a); temp
      }
    }
    def narrowUpdate(as: A*): Sketch[A] =
      Sketch.narrowUpdate(sketch, as.toList.map(a => (a, 1d)))
    def deepUpdate(as: A*): (Sketch[A], Option[Histogram[Double]]) =
      Sketch.deepUpdate(sketch, as.toList.map(a => (a, 1d)))
    def count(from: A, to: A): Double = Sketch.count(sketch, from, to)
    def sum: Double = Sketch.sum(sketch)
    def probability(from: A, to: A): Double =
      Sketch.probability(sketch, from, to)
    def rebuild: Sketch[A] =
      Sketch.rebuild(sketch)
    def cmapNo: Int = Sketch.cmapNo(sketch)
    def cmapSize: Int = Sketch.cmapSize(sketch)
    def counterNo: Int = Sketch.counterNo(sketch)
    def counterSize: Int = Sketch.counterSize(sketch)
    def youngCmap: Cmap = Sketch.youngCmap(sketch)
    def domain: RangeM[A] = Sketch.domain(sketch)
    def median: A = Sketch.median(sketch)
    def cdfPlot: PointPlot = Sketch.cdfSampling(sketch)
    def pointPdfSampling: PointPlot = Sketch.pdfSampling(sketch)
    def rangePdfSampling: DensityPlot = Sketch.rangePdfSampling(sketch)
    def barPlot: DensityPlot = Sketch.rangePdfSampling(sketch)
    def rangePlot: DensityPlot = Sketch.rangePdfSampling(sketch)
  }

}
