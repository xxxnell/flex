package flip.cmap

class UniformCmap(n: Int, start: Option[Double], end: Option[Double]) extends DividerCmap {

  val divider: List[Double] = {
    val defaultUnit = BigDecimal(0)
    val min = BigDecimal(start.getOrElse(DividerCmap.min))
    val max = BigDecimal(end.getOrElse(DividerCmap.max))

    val (idx, unit) = (start, end) match {
      case (Some(_), Some(_)) => (0 until (n - 1), if(n > 2) (max - min) / (n - 2) else defaultUnit)
      case (Some(_), None) => (0 until (n - 1), if(n > 2) (max - min) / (n - 1) else defaultUnit)
      case (None, Some(_)) => (1 until n, if(n > 2) (max - min) / (n - 1) else defaultUnit)
      case (None, None) => (1 until n, if(n > 2) (max - min) / n else defaultUnit)
    }

    idx.toList.map(idx => (min + idx * unit).toDouble)
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
