package semper.chalice2sil

import messages.Severity._
import scopt._
import chalice.{Chalice, PrintProgram}
import translation.ProgramTranslator
import java.io.File

object Program {
  // todo: fix! chalice options should pass as a string->string map

  def invokeChalice(opts: ProgramOptions): Option[scala.List[chalice.TopLevelDecl]] = {
    val chOptsBuilder = scala.collection.mutable.ArrayBuilder.make[String]()
    opts.chaliceOptions.foreach((entry) => {
      val (option, value) = entry
      if (value.isEmpty)
        chOptsBuilder += "/" + option
      else
        chOptsBuilder += "/" + option + ":" + value
    })
    chOptsBuilder += opts.chaliceFile
    val chOpts = chOptsBuilder.result()

    val chaliceParams = Chalice.parseCommandLine(chOpts) match {
      case Some(p) => p
      case None =>
        Console.out.println("Chalice failed to parse command line options. Chalice2SIL terminates.")
        return None
    }

    val program = Chalice.parsePrograms(chaliceParams) match {
      case Some(p) => p
      case None =>
        Console.out.println("Chalice program contained syntax errors. Chalice2SIL terminates.")
        return None //illegal program, errors have already been displayed
    }

    if (!chaliceParams.doTypecheck || !Chalice.typecheckProgram(chaliceParams, program)) {
      Console.out.println("Chalice program contained type errors. Chalice2SIL terminates.")
      return None
    }

    if (chaliceParams.printProgram) {
      Console.out.println("Program after type checking: ")
      PrintProgram.P(program)
    }
    Some(program)
  }

  def createTranslator(opts: ProgramOptions, program: Seq[chalice.TopLevelDecl]) = {
      // Define program name
    val ext = ".chalice"
    val n = new File(opts.chaliceFile).getName
    val programName = if (n.endsWith(ext)) n.dropRight(ext.length) else n

    new ProgramTranslator(opts, programName)
  }

  def translateToSil(opts: ProgramOptions, program: Seq[chalice.TopLevelDecl]):
      (semper.sil.ast.Program, Seq[semper.chalice2sil.Message]) = {
    val translator = createTranslator(opts, program)
    translator.translate(program)
  }

  def main(args: Array[String]) {
    val progOpts = new ProgramOptions()

    val cmdParser = new OptionParser("chalice2sil") {
      // Chalice file
      arg[File]("<chalice-file>")
        action { (source: String) => progOpts.chaliceFile = source }
        text ("The chalice source file.")

      // Options for Chalice
      opt[String]("chop")
        action { (opt: String) => progOpts.chaliceOptions = opt; Unit }
        text("Passes options to Chalice.")

      // Help
      help("?") text ("Displays this help message.")
    }

    def convertSlash(s: String): String =
      if (s.startsWith("/", 0))
        s.updated(0, '-')
      else
        s

    if (!cmdParser.parse(args.view.map(convertSlash))) {
      // Help has already been printed
      return;
    }

    val program = invokeChalice(progOpts) match {
      case None => return;
      case Some(p) => p
    }

    val (silProgram, messages) = translateToSil(progOpts, program)

    def pluralize(noun: String, count: Int) = count match {
      case 0 => "no " + noun + "s"
      case 1 => "1 " + noun
      case n => n.toString + " " + noun + "s"
    }

    Console.out.println(silProgram.toString())

    val warningCount = messages.count(_.severity == Warning)
    val errorCount = messages.count(_.severity.indicatesFailure)
    if (errorCount > 0)
      Console.out.println("[Failure] Chalice2SIL detected %s and %s.".format(
        pluralize("error", errorCount),
        pluralize("warning", warningCount)
      ))
    else
      Console.out.println("[Success] Chalice2SIL detected %s.".format(
        pluralize("warning", warningCount)
      ))
  }
}