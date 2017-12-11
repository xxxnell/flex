package sketch.scope.pdf.monad

import sketch.scope.pdf.{Dist, Sketch}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchBind[Sketch1[_]<:Sketch[_], D[_]<:Dist[_], Sketch2[_]<:Sketch[_]]
  extends SampleDistBind[Sketch1, D, Sketch2] {

}
