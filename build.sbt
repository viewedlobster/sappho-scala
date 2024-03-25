val scala3Version = "3.4.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "verona-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
