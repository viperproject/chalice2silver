package semper.chalice2sil

/**
Author: Yannis Kassios
*/

import semper.sil.testing.DefaultSilSuite
import semper.sil.verifier._
import semper.silicon.Silicon
import semper.sil.frontend._
import translation._
import java.nio.file.Path

class Chalice2SILFrontEnd extends DefaultPhases {
  var verf: Verifier = null
  var file: Path = null
  var chaliceAST: List[chalice.TopLevelDecl] = null
  var silAST: semper.sil.ast.Program = null
  var failed: Seq[AbstractError] = Seq()

  override def init(verifier: Verifier) { verf = verifier }

  override def reset(files: Seq[Path]) {
    chaliceAST = null
    silAST = null
    file = files(0)
    failed = Seq()
  }

  override def parse() {
    try {
      chaliceAST =
        chalice.Chalice.parsePrograms(
          chalice.Chalice.parseCommandLine(Array("-noVerify", file.toString)).get
        ).get
      Success
      } catch {
    case e =>
      // todo: enter message and position here
      val f = Seq(ParseError(e.toString, null))
      failed ++= f
      Failure(f)
     }
  }

  override def typecheck() {
    if(!failed.isEmpty && !chalice.Chalice.typecheckProgram(null, chaliceAST)) {
      // todo: enter message and position here
      val f = Seq(TypecheckerError("", null))
      failed ++= f
      Failure(f)
    }
    else Success
  }

  override def translate() {
    try { // note: warning messages from the translator are ignored!
      if (!failed.isEmpty) {
        val (s: semper.sil.ast.Program, _) = new ProgramTranslator(file.toString).translate(chaliceAST)
        silAST = s
      }
      Success
    } catch {
      case e =>
        val f = Seq(TranslationError(e.toString))
        failed ++= f
        Failure(f)
    }
  }

  override def verify() {
    val res = if (!failed.isEmpty) verf.verify(silAST) else Success
    if (res != Success) failed = res
    res
  }

  override def result() = if (!failed.isEmpty) Failure(failed) else Success
}

class AllTests extends DefaultSilSuite {

  override lazy val testDirectories: Seq[String] = Seq(
    "basic",
    "oldC2SCases",
    "chaliceSuite/examples",
    "chaliceSuite/general-tests",
    "chaliceSuite/permission-model",
    "chaliceSuite/predicates",
    "chaliceSuite/regressions",
    "chaliceSuite/substantial-examples",
    "quantificationOverPermissions"
  ).map("src/test/resources/" + _)

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new Chalice2SILFrontEnd()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override def verifiers: Seq[Verifier] = Seq(new Silicon())
}

 case class TranslationError(message: String) extends AbstractError {
   def fullId = "chalice2sil.error"
   def readableMessage = s"$message"
 }
