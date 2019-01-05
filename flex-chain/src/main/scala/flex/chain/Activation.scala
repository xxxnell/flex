package flex.chain

object Activation {

  def identity(x: Float): Float = ???

  def step(x: Float): Float = ???

  def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  def tanh(x: Float): Float = ???

  def relu(x: Float): Float = ???

}
