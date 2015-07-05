import bintray.Plugin._
import bintray.Keys._

name          := "util"
organization  := "me.hawkweisman"
version       := "0.0.3" // the current release version
scalaVersion  := "2.11.7"

sbtPlugin     := false // if we don't set this, Bintray will think
                       // we're releasing an SBT plugin

autoAPIMappings := true // link Scala standard lib in docs

libraryDependencies ++= Seq(
  "org.scalacheck"  %% "scalacheck" % "1.12.2"  % "test",
  "org.scalatest"   %% "scalatest"  % "2.2.4"   % "test"
)

bintraySettings ++ Seq(
  licenses += ("MIT", url("https://raw.githubusercontent.com/hawkw/scala-common/master/LICENSE")),
  publishMavenStyle := true,
  repository in bintray := "maven",
  bintrayOrganization in bintray := None,
  packageLabels in bintray := Seq("Scala", "Utilities")
)
