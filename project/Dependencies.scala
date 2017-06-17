import sbt._

object Dependencies {

  val snapshot = "-SNAPSHOT"

  val cats = Seq(
    "org.typelevel" %% "cats" % "0.9.0"
  )

  val monixs = Seq(
    "io.monix" %% "monix" % "2.2.2"
  )

  val specs2Version = "3.9.1"
  val specs = Seq(
    "org.specs2" %% "specs2-core" % specs2Version % "test",
    "org.specs2" %% "specs2-mock" % specs2Version % "test",
    "org.specs2" %% "specs2-junit" % specs2Version % "test",
    "org.specs2" %% "specs2-scalacheck" % specs2Version % "test"
  )

}