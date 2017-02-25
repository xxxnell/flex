import Dependencies._
import Resolvers._
import sbt.Keys._

name := "scope-sketch"

version := s"1.0$snapshot"

scalaVersion := "2.12.1"

val commonSettings = Seq(

  libraryDependencies ++= (cats ++ monixs),

  resolvers ++= typesafeRepo

)

lazy val scope_sketch = (project in file("."))
  .settings(
    libraryDependencies ++= (cats ++ monixs)
  )