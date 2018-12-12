package flex.pdf.update

import cats.data.NonEmptyList
import flex.conf.pdf.SmoothDistConf
import flex.pdf.arithmetic.CombinationDist
import flex.pdf.{Count, Dist, NumericDist, Prim}
import flex.measure.syntax._
import org.apache.commons.math3.special.Erf

object NormalSmoothingPs extends SmoothingPs {

  def apply(ps: List[(Prim, Count)], scale: Double): Dist[Prim] = {
    val conf = SmoothDistConf.default

    // todo head
    val inter = ps
      .sliding(3)
      .flatMap {
        case (p1, count1) :: (p2, count2) :: (p3, count3) :: Nil =>
          val diff1 = math.abs(p2 * count2 - p1 * count1) / (count2 + count1)
          val diff2 = math.abs(p2 * count2 - p3 * count3) / (count2 + count3)
          val avgDiff = (diff1 * count1 + diff2 * count3) / (count1 + count3)
          val variance = avgDiff / (math.sqrt(2) * Erf.erfInv(1 - scale))
          Some((count2, NumericDist.normal(p2, variance)(doubleMeasure, conf)))
        case _ => None
      }
      .toVector
    // todo last

    if (inter.nonEmpty) {
      val comps = NonEmptyList.fromList(inter.toList).get
      CombinationDist.apply(comps)(doubleMeasure, conf)
    } else NumericDist.uniform(0.0, Double.PositiveInfinity)(doubleMeasure, conf)
  }

}
