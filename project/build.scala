import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object Chalice2SilBuild extends Build {

  /* Base settings */
	
  lazy val baseSettings = (
       Defaults.defaultSettings
    ++ Seq(
          organization := "ch.ethz.inf.pm",
          version := "0.1-SNAPSHOT",
          // publishArtifact in packageDoc := false,
          scalaVersion := "2.10.0",
          // publishMavenStyle := false,
          // componentID := None,
          // crossPaths := false,
          // testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-w", "1"),
          // javacOptions in Compile ++= Seq("-target", "6", "-source", "6")
          scalacOptions in Compile ++= Seq("-deprecation", "-unchecked", "-feature"),
          resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"))

/* Projects */
          
  lazy val chalice2sil = Project(
    id = "chalice2sil",
    base = file("."),
    settings = (
         baseSettings
      ++ Seq(
            name := "Chalice2Sil",
            mainClass in (Compile, run) := Some("semper.chalice2sil.Program"),
            mainClass in assembly := Some("semper.chalice2sil.Program"),
            jarName in assembly := "chalice2sil.jar",
            test in assembly := {},
            testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
            traceLevel := 10,
            maxErrors := 6,
            classDirectory in Test <<= classDirectory in Compile,
            libraryDependencies ++= Seq(
              libs.scopt,
              libs.scalatest,
              libs.junit)))
  ).dependsOn(libs.chalice, libs.sil, libs.silicon)
  
  object libs {
    lazy val chalice = RootProject(new java.io.File("../Chalice"))
    lazy val sil = RootProject(new java.io.File("../Sil"))
    lazy val silicon = RootProject(new java.io.File("../Silicon"))

    lazy val scalatest = "org.scalatest" %% "scalatest" % "1.8" % "test" withJavadoc() withSources()
    lazy val scopt = "com.github.scopt" % "scopt_2.10" % "2.1.0"
    lazy val junit = "junit" % "junit" % "4.8.1" % "test"
      /* JUnit seems to only be required by semper.chalice2sil.util.UnicodeManglerTests. */

    /* TODO: Dependencies of Silicon. We should package Silicon such that the Silicon
     *       assembly includes all dependencies of Silicon.
     */
    lazy val slf4s = "com.weiglewilczek.slf4s" % "slf4s_2.9.1" % "1.0.7"
    lazy val slf4j = "org.slf4j" % "slf4j-log4j12" %	"1.6.4"
  }
}