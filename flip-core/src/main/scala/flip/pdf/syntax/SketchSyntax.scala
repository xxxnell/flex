package flip.pdf.syntax

import flip.cmap.Cmap
import flip.conf.pdf.SketchConf
import flip.measure.Measure
import flip.pdf.monad.{SketchBind, SketchFunctor}
import flip.pdf.{Count, Dist, Histogram, Sketch}
import flip.plot.{DensityPlot, PointPlot}
import flip.range.RangeM

trait SketchSyntax extends SketchPropSyntax with SketchMonadSyntax

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

trait SketchMonadSyntax extends SketchMonadSyntax1 {

  implicit class SketchMonadSyntaxImpl[A](sketch: Sketch[A]) {
    def map[B](f: A => B)(implicit functor: SketchFunctor[Sketch, Sketch, SketchConf],
                          measureB: Measure[B],
                          conf: SketchConf): Sketch[B] =
      functor.map(sketch, f, measureB, conf)
    def flatMap[B](f: A => Dist[B])(implicit bind: SketchBind[Sketch, Dist, Sketch, SketchConf],
                                    measureB: Measure[B],
                                    conf: SketchConf): Sketch[B] =
      bind.bind(sketch, f, measureB, conf)
  }

}

trait SketchMonadSyntax1 {

  implicit def sketchFunctor: SketchFunctor[Sketch, Sketch, SketchConf] = SketchFunctor.default
  implicit def sketchBind: SketchBind[Sketch, Dist, Sketch, SketchConf] = SketchBind.default

}
