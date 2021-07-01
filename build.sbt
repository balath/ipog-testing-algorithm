name := "ipog"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.25" % Test
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.25" % Test
libraryDependencies += "org.hammerlab.math" % "utils_2.12" % "2.4.0"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

unmanagedBase := baseDirectory.value / "lib"

scalacOptions += "--deprecation"