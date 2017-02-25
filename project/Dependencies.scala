import sbt._

object Dependencies {

  val snapshot = "-SNAPSHOT"

  val cats = Seq(
    "org.typelevel" %% "cats" % "0.9.0"
  )

  val monixs = Seq(
    "io.monix" %% "monix" % "2.2.2"
  )

}