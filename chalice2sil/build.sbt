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

libraryDependencies += "com.github.scopt" %% "scopt" % "2.0.1"

libraryDependencies += "pm.inf.ethz.ch" %% "silast" %  "0.1-SNAPSHOT"

libraryDependencies += "pm.inf.ethz.ch" %% "silicon" %  "0.1-SNAPSHOT"

libraryDependencies += "default" %% "chalice" %  "1.0"

// [Malte 2012-02-06]
//   Added the following two logging dependencies. Silicon depends on them,
//   and hence fails to run when invoked from Chalice2SIL.
//   It is probably more appropriate to bundle them with Silicon, but this
//   will do for now.
libraryDependencies += "com.weiglewilczek.slf4s" %% "slf4s" % "1.0.7"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" %	"1.6.4"

watchSources ++= (file("tests/regression/") ** "*").get
