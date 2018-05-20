package flip.conf.pdf

import flip.pdf.AdaSelSketch

trait AdaSelSketchConfB[+D <: AdaSelSketch[_]] extends AdaptiveSketchConfB[D] with SelectiveSketchConfB[D] {}
