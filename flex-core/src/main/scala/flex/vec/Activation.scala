package flex.vec

import org.nd4j.linalg.ops.transforms.Transforms

trait Activation {

  def step(x: Float): Float = if (x < 0) 0 else 1

  def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  def tanh(x: Float): Float = math.tanh(x).toFloat

  def relu(x: Float): Float = if (x < 0) 0 else x

}

trait ActivationSyntax {

  implicit class ActivationVecSyntaxImpl(vec: Vec) {
    def step: Vec = Transforms.tanh(vec)
    def sigmoid: Vec = Transforms.sigmoid(vec)
    def tanh: Vec = Transforms.tanh(vec)
    def relu: Vec = Transforms.relu(vec)
  }

}

object Activation extends Activation {

  object syntax extends ActivationSyntax

}
