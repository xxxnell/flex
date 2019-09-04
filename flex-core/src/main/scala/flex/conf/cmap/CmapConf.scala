package flex.conf.cmap

import flex.conf.pdf.IcdfSamplingConf
import flex.pdf.Prim

trait CmapConf extends IcdfSamplingConf {
  val size: Int
  val no: Int
}

object CmapConf {

  def uniformEqualize(
      size: Int,
      no: Int,
      start: Option[Prim],
      end: Option[Prim],
      boundaryRatio: Double): UniformEqualizeCmapConf =
    UniformEqualizeCmapConf(size, no, start, end, boundaryRatio)

}
