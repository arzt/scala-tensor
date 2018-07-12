import scalariform.formatter.preferences._

name := "scala-tensor"

version := "0.0.0-SNAPSHOT"

organization := "com.github.arzt"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.0.2" % "test",
  "org.jblas" % "jblas" % "1.2.4"
)

scalariformPreferences := scalariformPreferences.value
  .setPreference(DoubleIndentConstructorArguments, true)