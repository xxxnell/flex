package flip.cmap

class UniformCmap(n: Int, start: Option[Double], end: Option[Double]) extends DividerCmap {

  val divider: List[Double] = {
    val defaultUnit = BigDecimal(0)
    val minB = BigDecimal(start.getOrElse(DividerCmap.min))
    val maxB = BigDecimal(end.getOrElse(DividerCmap.max))

    val (idx, unitB) = (start, end) match {
      case (Some(_), Some(_)) => (0 until (n - 1), if(n > 2) (maxB - minB) / (n - 2) else defaultUnit)
      case (Some(_), None) => (0 until (n - 1), if(n > 2) (maxB - minB) / (n - 1) else defaultUnit)
      case (None, Some(_)) => (1 until n, if(n > 2) (maxB - minB) / (n - 1) else defaultUnit)
      case (None, None) => (1 until n, if(n > 2) (maxB - minB) / n else defaultUnit)
    }
    val min = minB.toDouble
    val unit = unitB.toDouble

    idx.toList.map(idx => min + idx * unit)
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
