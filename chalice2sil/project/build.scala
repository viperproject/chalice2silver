import sbt._
import Keys._

object HelloBuild extends Build {
  lazy val chalice2sil = Project(id = "chalice2sil",
                                 base = file(".")) dependsOn(chalice,silast,silicon)

  lazy val chalice = Project(id = "chalice",
                            base = file("boogie/Chalice"))
							
  lazy val silast = Project(id = "silast",
                            base = file("silicon/silast/src/SILAST"))

  lazy val silicon = Project(id = "silicon",
                         base = file("silicon")) dependsOn(silast)
}