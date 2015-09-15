import bintray.Plugin._
import bintray.Keys._

lazy val Benchmark = config("bench") extend Test

configs(Benchmark)

name          := "util"
organization  := "me.hawkweisman"
version       := "0.1.1" // the current release version
scalaVersion  := "2.11.7"

sbtPlugin     := false // if we don't set this, Bintray will think
                       // we're releasing an SBT plugin

autoAPIMappings := true // link Scala standard lib in docs

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
    "org.scalacheck"    %% "scalacheck" % "1.12.2"  % "test"
  , "org.scalatest"     %% "scalatest"  % "2.2.4"   % "test"
  , "com.storm-enroute" %% "scalameter" % "0.6"     % "bench"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

// ScalaMeter demands these settings due to reasons
logBuffered := false
parallelExecution in Benchmark := false
inConfig(Benchmark)(Defaults.testSettings)

bintraySettings ++ Seq(
    licenses += ("MIT",
      url("https://raw.githubusercontent.com/hawkw/scala-common/master/LICENSE")
      )
  , publishMavenStyle := true
  , repository in bintray := "maven"
  , bintrayOrganization in bintray := None
  , packageLabels in bintray := Seq("Scala", "Utilities")
)
