// Chalice2SIL

name := "chalice2sil"

organization := "ch.ethz.inf.pm"

version := "0.1-SNAPSHOT"

// Scala

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

classDirectory in Test <<= classDirectory in Compile

// Dependencies

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test" withJavadoc() withSources()

libraryDependencies += "junit" % "junit" % "4.+" % "test" withJavadoc()

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

libraryDependencies += "com.github.scopt" %% "scopt" % "1.1.+"