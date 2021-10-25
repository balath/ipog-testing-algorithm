name := "ipog"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

unmanagedBase := baseDirectory.value / "lib"

scalacOptions += "--deprecation"