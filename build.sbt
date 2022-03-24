name := "ipog"

version := "0.1"

scalaVersion := "2.13.6"
//val fs2Version = "3.2.0"
val munitVersion = "0.7.29"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.2.9" withSources() withJavadoc(),
  "com.nrinaudo" %% "kantan.csv" % "0.6.2",
  "org.scalameta" %% "munit" % munitVersion % Test,
  "org.scalameta" %% "munit-scalacheck" % munitVersion % Test,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

unmanagedBase := baseDirectory.value / "lib"

scalacOptions += "--deprecation"