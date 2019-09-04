package flex

import flex.chain.Complex
import flex.vec.SumVec
import org.deeplearning4j.nn.conf.distribution.Distribution
import org.deeplearning4j.nn.conf.layers.{ ConvolutionLayer, DenseLayer, SubsamplingLayer }

object CNN {

  def convInit(
      name: String,
      in: Int,
      out: Int,
      kernel: Array[Int],
      stride: Array[Int],
      pad: Array[Int],
      bias: Double): ConvolutionLayer =
    new ConvolutionLayer.Builder(kernel, stride, pad).name(name).nIn(in).nOut(out).biasInit(bias).build

  def conv3x3(name: String, out: Int, bias: Double): ConvolutionLayer =
    new ConvolutionLayer.Builder(Array[Int](3, 3), Array[Int](1, 1), Array[Int](1, 1))
      .name(name)
      .nOut(out)
      .biasInit(bias)
      .build

  def conv5x5(name: String, out: Int, stride: Array[Int], pad: Array[Int], bias: Double): ConvolutionLayer =
    new ConvolutionLayer.Builder(Array[Int](5, 5), stride, pad).name(name).nOut(out).biasInit(bias).build

  def maxPool(name: String, kernel: Array[Int]): SubsamplingLayer =
    new SubsamplingLayer.Builder(kernel, Array[Int](2, 2)).name(name).build

  def fullyConnected(name: String, out: Int, bias: Double, dropOut: Double, dist: Distribution): DenseLayer =
    new DenseLayer.Builder().name(name).nOut(out).biasInit(bias).dropOut(dropOut).dist(dist).build

}
