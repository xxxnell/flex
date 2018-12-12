package flex.cmap

class UniformCmap(n: Int, start: Option[Double], end: Option[Double]) extends DividerCmap {

  val divider: List[Double] = {
    val defaultUnit = 0
    val min = start.getOrElse(DividerCmap.min)
    val max = end.getOrElse(DividerCmap.max)

    def unitC(min: Double, max: Double, divider: Int): Double = {
      val c = (max - min) / divider
      if (!c.isInfinity) c else max * ((1 - min / max) / divider)
    }

    val (idx, unit) = (start, end) match {
      case (Some(_), Some(_)) => (0 until (n - 1), if (n > 2) unitC(min, max, n - 2) else defaultUnit)
      case (Some(_), None) => (0 until (n - 1), if (n > 2) unitC(min, max, n - 1) else defaultUnit)
      case (None, Some(_)) => (1 until n, if (n > 2) unitC(min, max, n - 1) else defaultUnit)
      case (None, None) => (1 until n, if (n > 2) unitC(min, max, n) else defaultUnit)
    }

    def dividingPoint(min: Double, idx: Int, unit: Double): Double = {
      val p = min + idx * unit
      if (!p.isInfinity) p else min * (1 + idx * (unit / min))
    }

    idx.toList.map(idx => dividingPoint(min, idx, unit))
  }

}

trait UniformCmapOps extends DividerCmapOps[UniformCmap]

object UniformCmap extends UniformCmapOps {

  /**
    * @param n number of pieces
    * */
  def apply(n: Int, start: Option[Double] = None, end: Option[Double] = None): UniformCmap =
    new UniformCmap(n, start, end)

}
