package flex.vec

import java.io.DataInputStream
import java.util.zip.DataFormatException

import monix.eval.Task

import scala.util.Try

/**
 * Some parts of this code is informed by the <a href="https://github.com/alno/scalann">scalann</a>.
 * */
object MNISTMan extends DatasetMan {

  val dname = "mnist"

  val trainIFname = "train-images-idx3-ubyte.gz"

  val trainOFname = "train-labels-idx1-ubyte.gz"

  val testIFname = "t10k-images-idx3-ubyte.gz"

  val testOFname = "t10k-labels-idx1-ubyte.gz"

  def url(fname: String) = s"http://yann.lecun.com/exdb/mnist/$fname"

  def readImage(is: DataInputStream): Try[List[Vec]] = Try {
    val (h, eh) = (is.readInt(), 2051)
    val count: Int = is.readInt()
    val width: Int = is.readInt()
    val height: Int = is.readInt()

    val images = List
      .range(0, count)
      .map(_ => List.range(0, height).flatMap(_ => List.range(0, width).map(_ => is.readUnsignedByte())))
    val vecs = images.map(image => Vec(image))

    if (h == eh) vecs else throw new DataFormatException(s"Head: $h, Expected: $eh")
  }

  def readLabel(is: DataInputStream): Try[List[Vec]] = Try {
    val (h, eh) = (is.readInt(), 2049)
    val count = is.readInt()

    val labels = List.range(0, count).map(_ => is.readByte().toInt)
    val vecs = labels.map(l => Vec((0 to 9).map(i => if (i == l) 1.0 else 0.0).toList))

    if (h == eh) vecs else throw new DataFormatException(s"Head: $h, Expected: $eh")
  }

  def readTrainImage: Task[List[Vec]] =
    get(dname, trainIFname, url(trainIFname)).flatMap(v => Task.fromTry(readImage(v)))

  def readTrainLabel: Task[List[Vec]] =
    get(dname, trainOFname, url(trainOFname)).flatMap(v => Task.fromTry(readLabel(v)))

  def readTestImage: Task[List[Vec]] =
    get(dname, testIFname, url(testIFname)).flatMap(v => Task.fromTry(readImage(v)))

  def readTestLabel: Task[List[Vec]] =
    get(dname, testOFname, url(testOFname)).flatMap(v => Task.fromTry(readLabel(v)))

}
