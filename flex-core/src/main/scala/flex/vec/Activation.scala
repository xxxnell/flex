package flex.vec

trait Activation {

  def step(x: Float): Float = ???

  def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  def tanh(x: Float): Float = ???

  def relu(x: Float): Float = ???

}

trait ActivationSyntax {

  implicit class ActivationVecSyntaxImpl(vec: Vec) {
    def step: Vec = ???
    def sigmoid: Vec = ???
    def tanh: Vec = ???
    def relu: Vec = ???
  }

}

object Activation extends Activation {

  object syntax extends ActivationSyntax

}
