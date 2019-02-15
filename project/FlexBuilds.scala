package flex

import flex.Resolvers._
import sbt.Keys._
import sbt._

object FlexBuilds {

  val buildSettings = Seq(
    scalaVersion := "2.12.8",
    organization := "com.xxxnell"
  )

  val defaultSettings = Seq(
    scalacOptions += "-Ypartial-unification",
    resolvers ++= typesafeRepo,
    fork := true,
    run / javaOptions ++= Seq(
      "-Xms8G",
      "-Xmx8G",
      "-Dorg.bytedeco.javacpp.maxbytes=8G",
      "-Dorg.bytedeco.javacpp.maxphysicalbytes=10G",
      "-Dorg.bytedeco.javacpp.openblas.load=mkl_rt"
    ),
    Test / javaOptions ++= Seq(
      "-Xms8G",
      "-Xmx8G",
      "-Dorg.bytedeco.javacpp.maxbytes=8G",
      "-Dorg.bytedeco.javacpp.maxphysicalbytes=10G",
      "-Dorg.bytedeco.javacpp.openblas.load=mkl_rt"
    )
  )

}
