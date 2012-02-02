// Chalice2SIL

name := "chalice2sil"

organization := "ch.ethz.inf.pm.semper"

version := "0.1"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

classDirectory in Test <<= classDirectory in Compile

libraryDependencies += "junit" % "junit" % "4.+" % "test" withJavadoc()

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

libraryDependencies += "com.github.scopt" %% "scopt" % "1.1.+"