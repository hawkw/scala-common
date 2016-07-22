# scala-common
[![Build Status](https://img.shields.io/travis/hawkw/scala-common/master.svg?style=flat)](https://travis-ci.org/hawkw/scala-common)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/7ba53eb29ba04e88b2126eefc716cb87)](https://www.codacy.com/app/hawk/scala-common?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=hawkw/scala-common&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/7ba53eb29ba04e88b2126eefc716cb87)](https://www.codacy.com/app/hawk/scala-common?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=hawkw/scala-common&amp;utm_campaign=Badge_Coverage)
[![Coverage](https://codecov.io/gh/hawkw/scala-common/branch/master/graph/badge.svg)](https://codecov.io/gh/hawkw/scala-common)
[![Dependency Status](https://www.versioneye.com/user/projects/57924291b7463b0037915d38/badge.svg?style=flat)](https://www.versioneye.com/user/projects/57924291b7463b0037915d38)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](http://doge.mit-license.org)
[![GitHub release](https://img.shields.io/github/release/hawkw/scala-common.svg?style=flat)](https://github.com/hawkw/scala-common/releases)
[![Bintray](https://img.shields.io/bintray/v/hawkw/maven/util.svg?style=flat)](https://bintray.com/hawkw/maven/util/_latestVersion)

General-purpose Scala code bits. Think of this as being like Apache Commons but little and in Scala.

### Building & Testing:

 + `$ sbt package` builds the release library Jar file
 + `$ sbt doc` generates the ScalaDoc API documentation
 + `$ sbt test` runs the unit test suite
 + `$ sbt bench:test` runs the ScalaMeter performance tests (currently just for `Linear`); be advised that these can  take Some Time to run.
