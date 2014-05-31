package semper.chalice2sil

/**
Author: Yannis Kassios
*/

import java.nio.file.Path
import semper.sil.testing._
import semper.sil.verifier._
import semper.silicon.Silicon
import semper.sil.frontend._

class AllTests extends SilSuite {
  override lazy val testDirectories: Seq[String] = Seq(
    "basic"/*,
    "oldC2SCases",
    "chaliceSuite/examples",
    "chaliceSuite/general-tests",
    "chaliceSuite/permission-model",
    "chaliceSuite/predicates",
    "chaliceSuite/regressions",
    "chaliceSuite/substantial-examples",
    "quantificationOverPermissions"*/
    // these test cases are removed from the test suite until they are annotated
  )

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new Chalice2SILFrontEnd()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override val verifiers = {
    val silicon = new Silicon()
    silicon.parseCommandLine(optionsFromScalaTestConfigMap())
    Seq(silicon)
  }

  override val defaultTestPattern: String = ".*\\.chalice"

  private def optionsFromScalaTestConfigMap(): Seq[String] = {
    val prefix = "silicon:"

    prefixSpecificConfigMap.get(prefix) match {
      case None => Seq()
      case Some(optionMap) => optionMap.flatMap{
        case (k, v) => Seq(s"--$k", v.toString)
      }.toSeq
    }
  }
}
