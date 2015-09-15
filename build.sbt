import bintray.Plugin._
import bintray.Keys._

lazy val Benchmark = config("bench") extend Test

name          := "util"
organization  := "me.hawkweisman"
version       := "0.1.1" // the current release version
scalaVersion  := "2.11.7"

sbtPlugin     := false // if we don't set this, Bintray will think
                       // we're releasing an SBT plugin

autoAPIMappings := true // link Scala standard lib in docs

// ScalaMeter is on SonaType
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.scalacheck"    %% "scalacheck" % "1.12.2"  % "test"
, "org.scalatest"     %% "scalatest"  % "2.2.4"   % "test"
, "com.storm-enroute" %% "scalameter" % "0.6"     % "bench"
)

//-- ScalaMeter performance testing settings ----------------------------------
configs(Benchmark)
val scalaMeter = new TestFramework("org.scalameter.ScalaMeterFramework")
testFrameworks in Benchmark += scalaMeter

logBuffered in Benchmark := false       // ScalaMeter demands these settings
parallelExecution in Benchmark := false // due to reasons

inConfig(Benchmark)(Defaults.testSettings)
testOptions in Benchmark += Tests.Argument(scalaMeter, "-silent")

//-- Bintray deployment settings ----------------------------------------------
bintraySettings ++ Seq(
  licenses += ("MIT"
   , url("https://raw.githubusercontent.com/hawkw/scala-common/master/LICENSE")
   )
, publishMavenStyle := true
, repository in bintray := "maven"
, bintrayOrganization in bintray := None
, packageLabels in bintray := Seq("Scala", "Utilities")
)
