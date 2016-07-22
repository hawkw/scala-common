import bintray.Plugin._
import bintray.Keys._
import sbt.Keys._

lazy val Benchmark = config("bench") extend Test

lazy val scalaMeter = new TestFramework("org.scalameter.ScalaMeterFramework")

lazy val scalatestVersion = "3.0.0-RC14"

// NOTE: we are locked in with this version of ScalaCheck, as newer versions
//       break compatibility with ScalaTest.
//       Refer to https://github.com/rickynils/scalacheck/issues/217 for more
//       information on this issue.
lazy val scalacheckVersion = "1.12.1"
//       I really wish this weren't the case; as I'd really like to be on the
//       latest stable version of all our dependencies, but, as usual, software
//       dependency management is a truly wretched Hell Zone of infinite sadness
//          - Eliza

lazy val commonSettings = Seq(
   organization    := "me.hawkweisman"
 , version         := "0.1.4" // the current release version
 , scalaVersion    := "2.11.8"
 , autoAPIMappings := true // link Scala standard lib in docs
 , sbtPlugin       := false // if we don't set this, Bintray will think
                            // we're releasing an SBT plugin
 , resolvers += "Sonatype OSS Snapshots" at // ScalaMeter is on SonaType
    "https://oss.sonatype.org/content/repositories/snapshots"
 , libraryDependencies ++= Seq(
      "org.scalactic"     %% "scalactic"  % scalatestVersion
    , "org.scalacheck"    %% "scalacheck" % scalacheckVersion % "test"
    , "org.scalatest"     %% "scalatest"  % scalatestVersion  % "test"
    , "com.storm-enroute" %% "scalameter" % "0.6"             % "bench"
    )

)

enablePlugins(VersionEyePlugin)

// VersionEyePlugin.projectSettings
existingProjectId in versioneye := "57924291b7463b0037915d38"
baseUrl in versioneye := "https://www.versioneye.com"
apiPath in versioneye := "/api/v2"
publishCrossVersion in versioneye := true


lazy val root = Project(
     "core"
   , file(".")
   , settings = Defaults.coreDefaultSettings ++
                              commonSettings ++
                              Seq(name := "util")
  ) configs (
    Benchmark
  ) settings (
    //-- ScalaMeter performance testing settings -----------------------------
    inConfig(Benchmark)(Defaults.testSettings ++ Seq(
        testOptions += Tests.Argument(scalaMeter, "-silent")
      , testFrameworks in Benchmark += scalaMeter
      , logBuffered in Benchmark := false       // ScalaMeter demands these settings
      , parallelExecution in Benchmark := false // due to reasons
    ))
  )

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
