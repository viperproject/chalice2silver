import sbt._
import Keys._

object Chalice2Sil extends Build {
  lazy val chalice2sil = Project(id = "chalice2sil",
                                 base = file(".")) 
}