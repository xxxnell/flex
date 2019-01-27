package flex.vec

import monix.eval.Task

trait Dataset {

  val in: List[Vec]

  val out: List[Option[Vec]]

}

trait DatasetOps {}

trait DatasetSyntax {

  implicit class DatasetSyntaxImpl(ds: Dataset) {}

}

object Dataset extends DatasetOps {

  object syntax extends DatasetSyntax

  private case class DatasetImpl(in: List[Vec], out: List[Option[Vec]]) extends Dataset

  def apply(in: List[Vec], out: List[Option[Vec]]): Dataset = DatasetImpl(in, out)

  def mnistTrain: Task[Dataset] =
    for {
      in <- MNISTMan.readTrainImage
      out <- MNISTMan.readTrainLabel
    } yield apply(in, out.map(Some(_)))

  def mnistTest: Task[Dataset] =
    for {
      in <- MNISTMan.readTestImage
      out <- MNISTMan.readTestLabel
    } yield apply(in, out.map(Some(_)))

}
