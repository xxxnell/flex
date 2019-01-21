import sbt.Keys._
import flex.FlexBuilds
import flex.Releases
import flex.Tasks

name := "flex"

lazy val root = project
  .in(file("."))
  .settings(moduleName := "root")
  .settings(Releases.publishSettings)
  .settings(Releases.noPublishSettings)
  .settings(Tasks.benchTaskSettings(flexBench))
  .aggregate(flexCore, flexChain, flexBench, flexDocs)

lazy val flexCore = flexModule("flex-core")
  .settings(moduleName := "flex", name := "flex core")
  .settings(Releases.publishSettings)

lazy val flexChain = flexModule("flex-chain")
  .settings(moduleName := "flex-chain", name := "flex chain")
  .settings(Releases.publishSettings)
  .dependsOn(flexCore)

lazy val flexBench = flexModule("flex-bench")
  .settings(moduleName := "flex-bench", name := "flex benchmarks")
  .settings(Releases.noPublishSettings)
  .dependsOn(flexCore, flexChain)

lazy val flexDocs = flexModule("flex-docs")
  .settings(moduleName := "flex-docs", name := "flex docs")
  .settings(Releases.noPublishSettings)

def flexModule(name: String): Project =
  Project(id = name, base = file(name))
    .settings(FlexBuilds.buildSettings)
