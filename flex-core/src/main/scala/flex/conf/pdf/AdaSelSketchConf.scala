package flex.conf.pdf

import flex.pdf.AdaSelSketch

trait AdaSelSketchConfB[+D <: AdaSelSketch[_]] extends AdaptiveSketchConfB[D] with SelectiveSketchConfB[D] {}
