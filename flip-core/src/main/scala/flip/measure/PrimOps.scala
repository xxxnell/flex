package flip.measure

import flip.pdf.Prim

object PrimOps {

  def similar(prim1: Prim, prim2: Prim, error: Double): Boolean = {
    if(prim1 != 0) Math.abs((prim1 - prim2) / prim1) <= error
    else if(prim2 != 0) Math.abs((prim1 - prim2) / prim2) <= error
    else true
  }

}

trait PrimSyntax {

  implicit val defaultError: Double = 0.05

  implicit class PrimSyntaxImpl(prim: Prim) {
    def ~=(prim2: Prim)(implicit error: Double): Boolean = PrimOps.similar(prim, prim2, error)
  }

}
