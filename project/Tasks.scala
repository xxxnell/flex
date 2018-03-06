package flip

import sbt._
import sbt.Keys._

object Tasks {

  lazy val experiment = taskKey[Unit]("Execute all experiments")

  def experimentTaskSetting(project: Project) = experiment := {
    val mains = (discoveredMainClasses in (project, Compile)).value
    val r = (runner in run).value
    val classpath: Seq[File] = (fullClasspath in (project, Compile)).value.files
    val logger = streams.value.log

    mains
      .filter(main => main.startsWith("flip.experiment"))
      .foreach(main => r.run(main, classpath, Seq(), logger))
  }

  def taskSettings(expProject: Project) = Seq(
    experimentTaskSetting(expProject)
  )

}
