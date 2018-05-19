package flip.conf

import flip.pdf.AdaSelSketch

trait AdaSelSketchConfB[+D <: AdaSelSketch[_]] extends AdaptiveSketchConfB[D] with SelectiveSketchConfB[D] {}
