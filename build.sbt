name := "ipog"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.2.9" withSources() withJavadoc(),
  "com.nrinaudo" %% "kantan.csv" % "0.6.2",
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

unmanagedBase := baseDirectory.value / "lib"

scalacOptions += "--deprecation"