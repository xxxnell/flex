import sbt.Keys._
import sketch.scope.Dependencies._
import sketch.scope.SketchBuilds

name := "scope-sketch"

lazy val root = Project(id = "scope-sketch", base = file("."))
  .aggregate(sketchCore, sketchBench)

lazy val sketchCore = sketchModule("scope-sketch-core")

lazy val sketchBench = sketchModule("scope-sketch-bench")
  .dependsOn(sketchCore)

def sketchModule(name: String): Project =
  Project(id = name, base = file(name))
    .settings(SketchBuilds.buildSettings)
