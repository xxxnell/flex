package flip.pdf.update

import flip.pdf.{Count, Dist, Prim}

trait SmoothingPs {

  def apply(ps: List[(Prim, Count)], scale: Double): Dist[Prim]

}
