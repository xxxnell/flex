package flex

import flex.chain.Complex
import flex.chain.Complex.syntax._
import flex.vec._
import org.deeplearning4j.nn.conf.{ GradientNormalization, NeuralNetConfiguration }
import org.deeplearning4j.nn.conf.inputs.InputType
import org.deeplearning4j.nn.conf.layers._
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.learning.config.Nesterovs
import org.nd4j.linalg.lossfunctions.LossFunctions
import org.nd4j.linalg.schedule.{ ScheduleType, StepSchedule }
import org.deeplearning4j.nn.conf.distribution._

object AlexNet {

  val (kin, kout) = (100, 10)

  val (k0, k1) = (20, 20)

  val (height, width, channel) = (100, 100, 3)

  val label = 10

  val mln = {
    val nonZeroBias = 1
    val dropOut = 0.5

    val conf = new NeuralNetConfiguration.Builder()
      .seed(40)
      .weightInit(WeightInit.DISTRIBUTION)
      .dist(new NormalDistribution(0.0, 0.01))
      .activation(Activation.RELU)
      .updater(new Nesterovs(new StepSchedule(ScheduleType.ITERATION, 1e-2, 0.1, 100000), 0.9))
      .biasUpdater(new Nesterovs(new StepSchedule(ScheduleType.ITERATION, 2e-2, 0.1, 100000), 0.9))
      .gradientNormalization(GradientNormalization.RenormalizeL2PerLayer)
      .l2(5 * 1e-4)
      .list
      .layer(0, CNN.convInit("cnn1", channel, 96, Array[Int](11, 11), Array[Int](4, 4), Array[Int](3, 3), 0))
      .layer(1, new LocalResponseNormalization.Builder().name("lrn1").build)
      .layer(2, CNN.maxPool("maxpool1", Array[Int](3, 3)))
      .layer(3, CNN.conv5x5("cnn2", 256, Array[Int](1, 1), Array[Int](2, 2), nonZeroBias))
      .layer(4, new LocalResponseNormalization.Builder().name("lrn2").build)
      .layer(5, CNN.maxPool("maxpool2", Array[Int](3, 3)))
      .layer(6, CNN.conv3x3("cnn3", 384, 0))
      .layer(7, CNN.conv3x3("cnn4", 384, nonZeroBias))
      .layer(8, CNN.conv3x3("cnn5", 256, nonZeroBias))
      .layer(9, CNN.maxPool("maxpool3", Array[Int](3, 3)))
      .layer(10, CNN.fullyConnected("ffn1", 4096, nonZeroBias, dropOut, new GaussianDistribution(0, 0.005)))
      .layer(11, CNN.fullyConnected("ffn2", 4096, nonZeroBias, dropOut, new GaussianDistribution(0, 0.005)))
      .layer(
        12,
        new OutputLayer.Builder(LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
          .name("output")
          .nOut(label)
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
    Complex.empty(kin, kout).addDim((channel * height * width) -> k0, mln.numParams.toInt -> k1).map(nn).init

}
