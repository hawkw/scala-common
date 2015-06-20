name                := "util"
organization        := "me.hawkweisman"
scalaVersion        := "2.11.6"
val projectVersion   = "0.0.2" // this is where you would set the current release version

val gitHeadCommitSha = settingKey[String]("current git commit short SHA")

gitHeadCommitSha in ThisBuild := Process("git rev-parse --short HEAD").lines.head

version in ThisBuild := s"$projectVersion-${gitHeadCommitSha.value}"

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
