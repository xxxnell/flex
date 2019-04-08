package flex

import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.vec._
import org.deeplearning4j.nn.conf.NeuralNetConfiguration
import org.deeplearning4j.nn.conf.inputs.InputType
import org.deeplearning4j.nn.conf.layers._
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.learning.config.Nesterovs
import org.nd4j.linalg.lossfunctions.LossFunctions

object LeNet {

  val (kin, kout) = (100, 100)

  val (k0, k1) = (20, 20)

  val (height, width, channel) = (100, 100, 3)

  val labelNo = 10

  val mln: MultiLayerNetwork = {
    val conf = new NeuralNetConfiguration.Builder()
      .seed(42)
      .l2(0.005)
      .activation(Activation.RELU)
      .weightInit(WeightInit.XAVIER)
      .updater(new Nesterovs(0.0001, 0.9))
      .list
      .layer(0, CNN.convInit("cnn1", channel, 50, Array[Int](5, 5), Array[Int](1, 1), Array[Int](0, 0), 0))
      .layer(1, CNN.maxPool("maxpool1", Array[Int](2, 2)))
      .layer(2, CNN.conv5x5("cnn2", 100, Array[Int](5, 5), Array[Int](1, 1), 0))
      .layer(3, CNN.maxPool("maxool2", Array[Int](2, 2)))
      .layer(4, new DenseLayer.Builder().nOut(500).build)
      .layer(5,
             new OutputLayer.Builder(LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
               .nOut(labelNo)
               .activation(Activation.SOFTMAX)
               .build)
      .backprop(true)
      .pretrain(false)
      .setInputType(InputType.convolutional(height, width, channel))
      .build

    val model = new MultiLayerNetwork(conf)
    model.init()

    model
  }

  def nn(input: SumVec): SumVec = input match {
    case dat :: lats =>
      mln.setParams(lats.concat)
      mln.output(dat.reshape(1, channel, height, width)).transpose :: Nil
  }

  def complex: Complex =
    Complex
      .empty(kin, kout)
      .addDim((channel * height * width) -> k0, mln.numParams.toInt -> k1)
      .map(nn)
      .init

}
