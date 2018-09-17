import sbt.Keys._
import flip.FlipBuilds
import flip.Releases
import flip.Tasks

name := "flip"

lazy val root = project
  .in(file("."))
  .settings(moduleName := "root")
  .settings(Releases.publishSettings)
  .settings(Releases.noPublishSettings)
  .settings(Tasks.benchTaskSettings(flipBench))
  .aggregate(flipCore, flipBench, flipDocs)

lazy val flipCore = flipModule("flip-core")
  .settings(moduleName := "flip", name := "Flip core")
  .settings(Releases.publishSettings)

lazy val flipBench = flipModule("flip-bench")
  .settings(moduleName := "flip-bench", name := "Flip benchmarks")
  .settings(Releases.noPublishSettings)
  .dependsOn(flipCore)

lazy val flipDocs = flipModule("flip-docs")
  .settings(moduleName := "flip-docs", name := "Flip docs")
  .settings(Releases.noPublishSettings)

def flipModule(name: String): Project =
  Project(id = name, base = file(name))
    .settings(FlipBuilds.buildSettings)
