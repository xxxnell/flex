package flip

import sbt.Keys._
import Dependencies._
import Resolvers._

object FlipBuilds {

  val buildSettings = Seq(
    scalaVersion := "2.12.4",
    organization := "com.xxxnell"
  )

  val defaultSettings = Seq(
    resolvers ++= typesafeRepo
  )

}
