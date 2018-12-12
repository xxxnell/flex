package flex.conf.cmap

import flex.conf.pdf.EqualizeIcdfSamplingConf
import flex.pdf.Prim

trait UniformEqualizeCmapConf extends UniformCmapConf with EqualizeIcdfSamplingConf

object UniformEqualizeCmapConf {

  private case class UniformEqualizeCmapConfImpl(
                                                 // CmapConf
                                                 size: Int,
                                                 no: Int,
                                                 // Uniform
                                                 start: Option[Prim],
                                                 end: Option[Prim],
                                                 // Equalized
                                                 boundaryRatio: Double)
      extends UniformEqualizeCmapConf

  def apply(size: Int,
            no: Int,
            start: Option[Prim],
            end: Option[Prim],
            boundaryRatio: Double): UniformEqualizeCmapConf =
    bare(size, no, start, end, boundaryRatio)

  def bare(size: Int, no: Int, start: Option[Prim], end: Option[Prim], boundaryRatio: Double): UniformEqualizeCmapConf =
    UniformEqualizeCmapConfImpl(size, no, start, end, boundaryRatio)

}
