
scalaVersion := "2.13.1"

name := "evolution-gaming-scala-bootcamp-homework"
organization := "ch.epfl.scala"
version := "1.0"

val catsVersion = "2.2.0"
val scalaTestVersion = "3.2.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.typelevel" %% "cats-core" % catsVersion
)


