package flip

import sbt.Keys.libraryDependencies
import sbt._

object Dependencies {

  val snapshot = "-SNAPSHOT"

  val cats = Seq(
    "org.typelevel" %% "cats" % "0.9.0"
  )

  val monixs = Seq(
    "io.monix" %% "monix" % "2.2.2"
  )

  val tics = Seq(
    "org.scalactic" %% "scalactic" % "3.0.3"
  )

  val datetime = Seq(
    "com.github.nscala-time" %% "nscala-time" % "2.16.0"
  )

  val specs2Version = "3.9.1"
  val specs = Seq(
    "org.specs2" %% "specs2-core",
    "org.specs2" %% "specs2-mock",
    "org.specs2" %% "specs2-junit",
    "org.specs2" %% "specs2-scalacheck"
  ).map(_ % specs2Version % Test)


  val math = Seq(
    "org.apache.commons" % "commons-math3" % "3.6.1"
  )

  // project dependencies

  val core = libraryDependencies ++= (cats ++ monixs ++ specs ++ tics ++ math)

  val bench = libraryDependencies ++= (specs ++ tics ++ datetime)

}
