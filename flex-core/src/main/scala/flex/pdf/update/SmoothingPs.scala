package flex.pdf.update

import flex.pdf.{ Count, Dist, Prim }

trait SmoothingPs {

  /**
   * @param scale The larger the scale, the smoother the input data list.
   *              The scale is defined in the interval 0 ≤ scale ≤ 1.
   * */
  def apply(ps: List[(Prim, Count)], scale: Double): Dist[Prim]

}
