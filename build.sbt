lazy val root = (project in file(".")).enablePlugins(GitVersioning)

name := "util"
organization := "me.hawkweisman"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalacheck"  %% "scalacheck" % "1.12.2"  % "test",
  "org.scalatest"   %% "scalatest"  % "2.2.4"   % "test"
)

bintraySettings ++ Seq(
  licenses += ("MIT", url("https://raw.githubusercontent.com/hawkw/scala-common/master/LICENSE")),
  publishMavenStyle := true,
  bintray.Keys.repository in bintray.Keys.bintray := "maven",
  bintray.Keys.bintrayOrganization in bintray.Keys.bintray := None,
  bintray.Keys.packageLabels in bintray.Keys.bintray := Seq("Scala", "Utilities")
)