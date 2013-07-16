package semper.chalice2sil

import messages.Severity._
import scopt._
import chalice.{Chalice, PrintProgram}
import translation.ProgramTranslator
import java.io.File

case class ProgramOptions (chaliceOptions : Map[String, String] = new scala.collection.immutable.HashMap[String, String],
                           chaliceFile : java.io.File = null)

object Program {
  def invokeChalice(opts: ProgramOptions): Option[scala.List[chalice.TopLevelDecl]] = {
    val chOptsBuilder = scala.collection.mutable.ArrayBuilder.make[String]()
    opts.chaliceOptions.foreach((entry) => {
      val (option, value) = entry
      if (value.isEmpty)
        chOptsBuilder += "/" + option
      else
        chOptsBuilder += "/" + option + ":" + value
    })
    chOptsBuilder += opts.chaliceFile.getAbsolutePath
    val chOpts = chOptsBuilder.result()
    val chaliceOptions = Chalice.parseCommandLine(chOpts)

    val chaliceOptions1 = chaliceOptions match {
      case Some(c) => c
      case None =>
        Console.out.println("Chalice could not parse its options.  Chalice2SIL terminates")
        return None
    }

    val program = Chalice.parsePrograms(chaliceOptions1) match {
      case Some(p) => p
      case None =>
        Console.out.println("Chalice program contained syntax errors. Chalice2SIL terminates.")
        return None //illegal program, errors have already been displayed
    }

    if (!Chalice.typecheckProgram(chaliceOptions1, program)) {
      Console.out.println("Chalice program contained type errors. Chalice2SIL terminates.")
      return None
    }

    Some(program)
  }

  def createTranslator(opts: ProgramOptions, program: Seq[chalice.TopLevelDecl]) = {
      // Define program name
    val ext = ".chalice"
    val n = opts.chaliceFile.getName
    val programName = if (n.endsWith(ext)) n.dropRight(ext.length) else n

    new ProgramTranslator(opts, programName)
  }

  def translateToSil(opts: ProgramOptions, program: Seq[chalice.TopLevelDecl]):
      (semper.sil.ast.Program, Seq[semper.chalice2sil.Message]) = {
    val translator = createTranslator(opts, program)
    translator.translate(program)
  }

  def main(args: Array[String]) {
    val cmdParser = new OptionParser[ProgramOptions]("chalice2sil") {
      // Chalice file
      arg[File]("<chalice-file>") action {
        (source, c) => c.copy(chaliceFile = source)
      } text ("The chalice source file.")

      // Options for Chalice
      opt[(String, String)]("chop") action {
        case ((opt, value), c) => c.copy(chaliceOptions = c.chaliceOptions + (opt -> value ))
      } text("Passes options to Chalice.")

      // Help
      help("?") text ("Displays this help message.")
    }

    var progOptions : ProgramOptions = cmdParser.parse(args, ProgramOptions()) map { opts => opts } getOrElse {
      Console.out.println("Option parsing failed.  Run with -? for more details.")
      return
    }.asInstanceOf[ProgramOptions]
    progOptions = progOptions.copy(chaliceOptions = progOptions.chaliceOptions + ("noVerify" -> ""))

    val chaliceProg = invokeChalice(progOptions)
    if (chaliceProg == None) return
    val chaliceProg1 = chaliceProg match { case Some(c) => c }
    val (silProgram, messages) = translateToSil(progOptions, chaliceProg1)

    def pluralize(noun: String, count: Int) = count match {
      case 0 => "no " + noun + "s"
      case 1 => "1 " + noun
      case n => n.toString + " " + noun + "s"
    }

    Console.out.println(silProgram)

    val warningCount = messages.count(_.severity == Warning)
    val errorCount = messages.count(_.severity.indicatesFailure)
    if (errorCount > 0)
      Console.err.println("[Failure] Chalice2SIL detected %s and %s.".format(
        pluralize("error", errorCount),
        pluralize("warning", warningCount)
      ))
    else
      Console.err.println("[Success] Chalice2SIL detected %s.".format(
        pluralize("warning", warningCount)
      ))
    messages.foreach(m => Console.out.println(m))
  }
}