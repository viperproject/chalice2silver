package semper.chalice2sil

import java.nio.file.Path
import semper.sil.verifier._
import semper.sil.frontend._
import semper.sil.ast.{NoPosition, Position}
import chalice.Chalice
import translation._
import semper.chalice2sil.messages.ReportMessage

/* TODO: Extend SilFrontend, which provides goodies such as error reporting and
 *       help printing, which is currently done outside of Chalice2SILFrontEnd,
 *       i.e., in chalice2sil.Program.
 */
class Chalice2SILFrontEnd extends DefaultPhases {
  type ChaliceProgram = List[chalice.TopLevelDecl]

  var verf: Verifier = null
  var file: Path = null
  var chaliceAST: List[chalice.TopLevelDecl] = null
  var silAST: semper.sil.ast.Program = null
  var failed: Seq[AbstractError] = Seq()
  var messages = Seq[ReportMessage]()
  var verifierResult: VerificationResult = null

  override def init(verifier: Verifier) { verf = verifier }

  override def reset(files: Seq[Path]) {
    chaliceAST = null
    silAST = null
    file = files(0)
    failed = Seq()
  }

  protected def parseChaliceCommandLine(): Either[Chalice.CommandLineParameters, AbstractError] = {
    val commandLine = Array("-noVerify", file.toString)

    Chalice.parseCommandLine(commandLine) match {
      case Some(c) => Left(c)
      case None => Right(CliOptionError(commandLine.mkString(" ")))
    }
  }

  protected def parseChaliceProgram(options: Chalice.CommandLineParameters): Either[ChaliceProgram, AbstractError] = {
    Chalice.parsePrograms(options) match {
      case Some(p) => Left(p)
      case None => Right(ParseError("Chalice program contained syntax errors.", NoPosition)) /* TODO: Add message and position */
    }
  }

  override def parse() {
    try {
      parseChaliceCommandLine().left.map(options => parseChaliceProgram(options)).joinLeft match {
        case Left(program) =>
          chaliceAST = program
          Success

        case Right(error) =>
          val failures = Seq(error)
          failed ++= failures
          Failure(failures)
      }
    } catch {
      case e: Throwable =>
        // todo: enter message and position here
        val f = Seq(ParseError(e.toString, NoPosition))
        failed ++= f
        Failure(f)
    }
  }

  override def typecheck() {
    try {
      if(failed.isEmpty && !chalice.Chalice.typecheckProgram(null, chaliceAST)) {
        // todo: enter message and position here
        val f = Seq(TypecheckerError("", NoPosition))
        failed ++= f
        Failure(f)
      }
      else Success
    } catch {
        case e: Throwable =>
          val f = Seq(TypecheckerError(e.toString, NoPosition))
          failed ++= f
          Failure(f)
    }
  }

  override def translate() {
    try {
      // note: warning messages from the translator are ignored!
      if (failed.isEmpty) {
        val (s: semper.sil.ast.Program, messages) = new ProgramTranslator(file.toString).translate(chaliceAST)
        silAST = s
        this.messages = messages
      }
      Success
    } catch {
        case e: Throwable =>
          // todo: enter message and position here
          val f = Seq(TranslationError(e.toString, NoPosition))
          failed ++= f
          Failure(f)
    }
  }

  override def verify() {
    try {
      verifierResult =
        if (failed.isEmpty && verf != null) verf.verify(silAST)
        else Success
    } catch {
        case e: Throwable =>
          val f = Seq(VerifierThrowsException(e.toString))
          failed ++= f
          Failure(f)
    }
  }

  override def result = if (!failed.isEmpty) Failure(failed) else Success
}

case class TranslationError(message: String, pos: Position) extends AbstractError {
  def fullId = "chalice2sil.error"
  def readableMessage = s"$message"
}

case class VerifierThrowsException(message: String) extends AbstractError {
  def fullId = "verifier.exception"
  def readableMessage = s"$message"
  val pos = NoPosition
}
