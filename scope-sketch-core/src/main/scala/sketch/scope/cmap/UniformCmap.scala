package sketch.scope.cmap

/**
  * Licensed by Probe Technology, Inc.
  */
class UniformCmap(n: Int, start: Option[Double], end: Option[Double]) extends DividerCmap {

  val divider: List[Double] = {
    val min = BigDecimal(start.getOrElse(DividerCmap.min))
    val max = BigDecimal(end.getOrElse(DividerCmap.max))
    val unit = if(n > 0) (max - min) / n else max - min

    val idx = (start, end) match {
      case (Some(_), Some(_)) => 0 until (n - 1)
      case (Some(_), None) => 0 until (n - 1)
      case (None, Some(_)) => 1 until n
      case (None, None) => 1 until n
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
