import scalariform.formatter.preferences._

name := "scala-tensor"

version := "0.0.1-SNAPSHOT"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")

organization := "com.github.arzt"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.6.0" % "test",
  "org.jblas" % "jblas" % "1.2.4",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.2"
)

scalacOptions ++= Seq("-target:jvm-1.8") //needed for scala 2.11

crossScalaVersions := List("2.13.1", "2.12.10", "2.11.12")


scalariformPreferences := scalariformPreferences.value
  .setPreference(DoubleIndentConstructorArguments, true)