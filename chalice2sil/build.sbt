// Chalice2SIL

import AssemblyKeys._

name := "chalice2sil"

organization := "ch.ethz.inf.pm"

version := "0.1-SNAPSHOT"

mainClass in assembly := Some("ch.ethz.inf.pm.semper.chalice2sil.Program")

// sbt-assembly (https://github.com/sbt/sbt-assembly)

assemblySettings

jarName in assembly := "chalice2sil.jar"

test in assembly := {}

// Scala

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

classDirectory in Test <<= classDirectory in Compile

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

// Dependencies

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test" withJavadoc() withSources()

libraryDependencies += "com.github.scopt" %% "scopt" % "1.1.3"
