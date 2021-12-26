import scalariform.formatter.preferences._

name := "scala-tensor"

version := "0.0.1-SNAPSHOT"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions"
)

organization := "com.github"

scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.10" % "test",
  "org.jblas" % "jblas" % "1.2.4",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0",
  "net.java.dev.jna" % "jna" % "5.5.0",
  "net.java.dev.jna" % "jna-platform" % "5.5.0"
)

crossScalaVersions := List("3.1.0", "3.0.2", "2.13.7", "2.12.15")

scalariformPreferences := scalariformPreferences.value
  .setPreference(DoubleIndentConstructorArguments, true)
