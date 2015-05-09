name := "util"
organization := "me.hawkweisman"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalacheck"  %% "scalacheck" % "1.12.2"  % "test",
  "org.scalatest"   %% "scalatest"  % "2.2.4"   % "test"
)

licenses += ("MIT", url("https://raw.githubusercontent.com/hawkw/scala-common/master/LICENSE"))