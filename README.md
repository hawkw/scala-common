# scala-common
[![Build Status](https://img.shields.io/travis/hawkw/scala-common/master.svg?style=flat-square)](https://travis-ci.org/hawkw/scala-common)
[![Codacy Badge](https://img.shields.io/codacy/7ba53eb29ba04e88b2126eefc716cb87.svg?style=flat-square)](https://www.codacy.com/app/hawk/scala-common)
[![Coverage](https://codecov.io/gh/hawkw/scala-common/branch/master/graph/badge.svg)](https://codecov.io/gh/hawkw/scala-common))
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat-square)](http://doge.mit-license.org)
[![GitHub release](https://img.shields.io/github/release/hawkw/scala-common.svg?style=flat-square)](https://github.com/hawkw/scala-common/releases)
[![Bintray](https://img.shields.io/bintray/v/hawkw/maven/util.svg?style=flat-square)](https://bintray.com/hawkw/maven/util/_latestVersion)

General-purpose Scala code bits. Think of this as being like Apache Commons but little and in Scala.

### Building & Testing:

 + `$ sbt package` builds the release library Jar file
 + `$ sbt doc` generates the ScalaDoc API documentation
 + `$ sbt test` runs the unit test suite
 + `$ sbt bench:test` runs the ScalaMeter performance tests (currently just for `Linear`); be advised that these can  take Some Time to run.
