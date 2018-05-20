package flip.conf

import flip.pdf._

package object pdf {

  type DistConf = DistConfB[Dist[_]]

  type SmoothDistConf = SmoothDistConfB[SmoothDist[_]]

  type SamplingDistConf = SamplingDistConfB[SamplingDist[_]]

  type DataBinningDistConf = DataBinningDistConfB[DataBinningDist[_]]

  type SketchConf = SketchConfB[Sketch[_]]

  type PeriodicSketchConf = PeriodicSketchConfB[PeriodicSketch[_]]

  type AdaptiveSketchConf = AdaptiveSketchConfB[AdaptiveSketch[_]]

  type AdaPerSketchConf = AdaPerSketchConfB[AdaPerSketch[_]]

  type SelectiveSketchConf = SelectiveSketchConfB[SelectiveSketch[_]]

  type AdaSelSketchConf = AdaSelSketchConfB[AdaSelSketch[_]]

}
