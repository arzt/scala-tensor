import scalariform.formatter.preferences._

name := "scala-tensor"

version := "0.0.0-SNAPSHOT"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.0.2" % "test",
  "com.github.fommil.netlib" % "all" % "1.1.2"
)

scalariformPreferences := scalariformPreferences.value
  .setPreference(DoubleIndentConstructorArguments, true)