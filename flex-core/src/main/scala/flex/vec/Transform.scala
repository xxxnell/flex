package flex.vec

import org.nd4j.linalg.ops.transforms.Transforms

trait Transform {

  def step(x: Float): Float = if (x < 0) 0 else 1

  def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  def tanh(x: Float): Float = math.tanh(x).toFloat

  def relu(x: Float): Float = if (x < 0) 0 else x

}

trait TransformSyntax {

  implicit class TransformSyntaxImpl(x: Vec) {
    def step: Vec = Transforms.tanh(x)
    def sigmoid: Vec = Transforms.sigmoid(x)
    def tanh: Vec = Transforms.tanh(x)
    def relu: Vec = Transforms.relu(x)
    def floor: Vec = Transforms.floor(x)
    def round: Vec = Transforms.round(x)
  }

}

object Transform extends Transform {

  object syntax extends TransformSyntax

}
