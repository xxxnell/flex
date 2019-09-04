package flex.vec

import java.io.{ DataInputStream, File, FileInputStream, FileOutputStream }
import java.net.URL
import java.nio.channels.Channels
import java.nio.file.{ Files, Paths }
import java.util.zip.GZIPInputStream

import monix.eval.Task

/**
 * Some parts of this code is informed by the <a href="https://github.com/alno/scalann">scalann</a>.
 * */
trait DatasetMan {

  /**
   * @param dname Dataset name
   * @param fname File name
   * @param url Dataset link
   * */
  def get(dname: String, fname: String, url: String): Task[DataInputStream] = Task {
    val dir = Option(System.getenv("DATASET_PATH"))
      .getOrElse((Paths.get(".").toString :: ".cache" :: dname :: Nil).mkString(File.separator))
    if (!Files.exists(Paths.get(dir))) new File(dir).mkdirs

    val path = Paths.get(dir, fname)
    if (!Files.exists(path)) {
      val rbc = Channels.newChannel(new URL(url).openStream())
      val fos = new FileOutputStream(path.toString)
      fos.getChannel.transferFrom(rbc, 0, Long.MaxValue)
    }

    new DataInputStream(new GZIPInputStream(new FileInputStream(path.toString)))
  }

}
