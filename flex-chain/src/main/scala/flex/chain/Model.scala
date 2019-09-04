package flex.chain

trait Model {}

trait ModelOps {}

trait ModelSyntax {

  implicit class ModelSyntaxImpl(model: Model) {}

}

object Model extends ModelOps {

  object syntax extends ModelSyntax

}
