package sketch.scope.measure

import sketch.scope.pdf.Prim

/**
  * Licensed by Probe Technology, Inc.
  */
object PrimOps {

  def similar(prim1: Prim, prim2: Prim, error: Double): Boolean = Math.abs((prim1 - prim2) / prim1) <= error

}

trait PrimSyntax {

  implicit val defaultError: Double = 0.05

  implicit class PrimSyntaxImpl(prim: Prim) {
    def ~=(prim2: Prim)(implicit error: Double): Boolean = PrimOps.similar(prim, prim2, error)
  }

}
