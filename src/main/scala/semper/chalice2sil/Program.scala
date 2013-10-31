/**
 * Author: Yannis Kassios (based on an older version by Christian Klauser)
 */

package semper.chalice2sil

import scopt._
import chalice.{Chalice, PrintProgram}
import translation.ProgramTranslator
import java.io.File

// **
// Options to pass to the Chalice library; together with an optional filename
// This translation parameter is currently not used, except to pass the -noVerify flag.
// It may be removed in the future, unless a need to pass options to Chalice shows up.
// **
case class ProgramOptions(chaliceOptions : Map[String, String] = new scala.collection.immutable.HashMap[String, String],
                          chaliceFile : java.io.File = null)

// **
// Translate one file from command line
// **
object Program {
  // **
  // Invokes Chalice and, if parsing and resolution are successful, returns a Chalice AST
  // **
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
        return None
    }

    if (!Chalice.typecheckProgram(chaliceOptions1, program)) {
      Console.out.println("Chalice program contained type errors. Chalice2SIL terminates.")
      return None
    }

    Some(program)
  }

  // **
  // Main
  // **
  def main(args: Array[String]) {
    var chaliceFileName : String = null

    // Option parser
    val cmdParser = new OptionParser[ProgramOptions]("chalice2sil") {
      // Chalice file
      arg[File]("<chalice-file>") action {
        (source, c) => chaliceFileName = source.toString ; c.copy(chaliceFile = source)
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
      // we don't want Chalice to call Boogie

    // Translate
    val chaliceProg = invokeChalice(progOptions)
    if (chaliceProg == None) return
    val chaliceProg1 = chaliceProg match { case Some(c) => c }
    val (silProgram, messages) = new ProgramTranslator(chaliceFileName).translate(chaliceProg1)
    def pluralize(noun: String, count: Int) = count match {
      case 0 => "no " + noun + "s"
      case 1 => "1 " + noun
      case n => n.toString + " " + noun + "s"
    }

    // Report
    Console.out.println(silProgram)
    messages.foreach(m => Console.err.println("[Chalice2SIL] " + m))
    Console.err.println("Chalice2SIL produced %s.".format(pluralize("message", messages.length)))
  }
}