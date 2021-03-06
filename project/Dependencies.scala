package flex

import sbt._
import sbt.Keys.libraryDependencies

object Dependencies {

  val cats = Seq(
    "org.typelevel" %% "cats-core" % Versions.cats
  )

  val monixs = Seq(
    "io.monix" %% "monix",
    "io.monix" %% "monix-execution"
  ).map(_ % Versions.monix)

  val tics = Seq(
    "org.scalactic" %% "scalactic" % Versions.scalactic
  )

  val datetime = Seq(
    "com.github.nscala-time" %% "nscala-time" % Versions.nscalaTime
  )

  val specs2Version = Versions.specs2
  val specs = Seq(
    "org.specs2" %% "specs2-core",
    "org.specs2" %% "specs2-mock",
    "org.specs2" %% "specs2-junit",
    "org.specs2" %% "specs2-scalacheck"
  ).map(_ % specs2Version % Test)

  val math = Seq(
    "org.apache.commons" % "commons-math3" % Versions.commonsMath3
  )

  val jol = Seq(
    "org.openjdk.jol" % "jol-core" % Versions.jol
  )

  val csv = Seq(
    "com.github.tototoshi" %% "scala-csv" % Versions.csv
  )

  val nd4j = Seq(
//    "org.nd4j" % "nd4j-native-platform" % Versions.dl4j
    "org.nd4j" % "nd4j-cuda-9.0-platform" % Versions.dl4j
  )

  val dl4j = Seq(
    "org.deeplearning4j" % "deeplearning4j-core" % Versions.dl4j
  )

  // project dependencies

  val core = libraryDependencies ++= (cats ++ monixs ++ specs ++ tics ++ math ++ nd4j)

  val chain = libraryDependencies ++= (cats ++ monixs ++ specs ++ tics ++ math ++ nd4j ++ dl4j)

  val bench = libraryDependencies ++= (specs ++ tics ++ datetime ++ jol ++ csv)

}
