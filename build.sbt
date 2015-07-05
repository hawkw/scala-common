name                := "util"
organization        := "me.hawkweisman"
scalaVersion        := "2.11.7"

// versioning stuff
val projectVersion   = "0.0.3" // the current release version
val gitHeadCommitSha = settingKey[String]("current git commit short SHA")

gitHeadCommitSha in ThisBuild := Process("git rev-parse --short HEAD")
  .lines
  .headOption
  .getOrElse("")

version in ThisBuild := s"$projectVersion-${gitHeadCommitSha.value}"

autoAPIMappings := true

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
