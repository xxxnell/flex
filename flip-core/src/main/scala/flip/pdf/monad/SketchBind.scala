package flip.pdf.monad

import flip.conf.{SketchConf, SketchConfB}
import flip.pdf.{Dist, Sketch}

import scala.language.higherKinds

trait SketchBind[D1[_] <: Sketch[_], D2[_] <: Dist[_], D3[_] <: Sketch[_], C <: SketchConfB[D3[_]]]
    extends SamplingDistBind[D1, D2, D3, C] {}

object SketchBind {

  def default: SketchBind[Sketch, Dist, Sketch, SketchConf] = PointToPointSketchBind

}
