package flip

import sbt._
import sbt.Keys._
import bintray.BintrayKeys._
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

import scala.sys.process._
import language.postfixOps

trait ReleaseProcess {

  def developBranch = "develop"

  def mainBranch: String = {
    sys.env.getOrElse("TRAVIS_BRANCH", currentBranch)
  }

  def currentBranch: String = {
    val parsed: String = { "git rev-parse --abbrev-ref HEAD".!!.trim }

    parsed
  }

  def checkout(name: String) =
    ReleaseStep(action = state => {
      { s"echo checkout to $name" ! }
      val res = { s"git checkout $name" ! }

      if (res != 0) {
        sys.error(s"Checkout error occurs.")
      }

      state
    })

  def merge(name: String) =
    ReleaseStep(action = state => {
      { s"echo Merging $name" ! }
      val res = { s"git merge $name" ! }

      if (res != 0) {
        sys.error(s"Conflict occurs: -> $name")
      }

      state
    })

  def push(name: String) =
    ReleaseStep(action = state => {
      { s"echo Pushing $name branch." ! }
      val res = { s"git push origin $name" ! }

      if (res != 0) {
        sys.error(s"Push error occurs: branch $name")
      }

      state
    })

  def pushTags =
    ReleaseStep(action = state => {
      { "echo Pushing tags." ! }
      val res = { "git push --tags" ! }

      if (res != 0) {
        sys.error("Push Tag error occurs.")
      }

      state
    })

}

object Releases extends ReleaseProcess {

  lazy val noPublishSettings = Seq(
    publish := {},
    publishLocal := {},
    publishArtifact := false
  )

  lazy val publishSettings = Seq(
    homepage := Some(url("https://github.com/xxxnell/flip")),
    licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
    // bintray
    bintrayRepository := "oss-maven",
    // release
    releaseIgnoreUntrackedFiles := true,
    releaseTagComment := s"+ releasing ${(version in ThisBuild).value}",
    releaseCommitMessage := s"* [skip ci] setting version to ${(version in ThisBuild).value}",
    releaseProcess := releaseSteps
  )

  lazy val releaseSteps: Seq[ReleaseStep] = Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    checkout(mainBranch),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    checkout(developBranch),
    merge(mainBranch),
    setNextVersion,
    commitNextVersion,
    push(mainBranch),
    pushTags,
    push(developBranch)
  )

}
