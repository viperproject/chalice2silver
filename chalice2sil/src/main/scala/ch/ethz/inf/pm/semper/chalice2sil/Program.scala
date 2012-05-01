package ch.ethz.inf.pm.semper.chalice2sil

import messages.Severity._
import scopt._
import chalice.{Chalice,PrintProgram}
import translation.ProgramTranslator
import java.io.File
import silAST.source.noLocation
import ch.ethz.inf.pm.silicon.Config._
import ch.ethz.inf.pm.silicon.{Silicon, Config}

object Program {

  def invokeChalice(opts : ProgramOptions) : Option[scala.List[chalice.TopLevelDecl]] = {
    val chOptsBuilder = scala.collection.mutable.ArrayBuilder.make[String]()
    opts.chaliceOptions.foreach((entry) => {
      val (option, value) = entry
      if(value.isEmpty)
        chOptsBuilder += "/" + option
      else
        chOptsBuilder += "/" + option + ":" + value
    })
    chOptsBuilder ++= opts.chaliceFiles
    val chOpts = chOptsBuilder.result()

    println("Forwarding to Chalice (" + chOpts.length + ")")
    chOpts.foreach((s) => {
      print(" "); print(s)
    })
    println();

    val chaliceParams = Chalice.parseCommandLine(chOpts) match {
      case Some(p) => p
      case None =>
        if(opts.verbose)
          Console.out.println("Chalice failed to parse command line options. Chalice2SIL terminates.");
        return None;
    }

    val program = Chalice.parsePrograms(chaliceParams) match {
      case Some(p) => p
      case None =>
        if(opts.verbose)
          Console.out.println("Chalice program contained syntax errors. Chalice2SIL terminates.");
        return None; //illegal program, errors have already been displayed
    }

    if(!chaliceParams.doTypecheck || !Chalice.typecheckProgram(chaliceParams, program)) {
      if(opts.verbose)
        Console.out.println("Chalice program contained type errors. Chalice2SIL terminates.")
      return None;
    }

    if(chaliceParams.printProgram) {
      Console.out.println("Program after type checking: ");
      PrintProgram.P(program)
    }
    Some(program)
  }

  def createTranslator(opts : ProgramOptions, program : scala.List[chalice.TopLevelDecl]) = {
    // Translate to SIL
    val programName = opts.chaliceFiles.headOption.map(p => {
      val ext = ".chalice"
      val n = new File(p).getName
      if(n.endsWith(ext))
        n.dropRight(ext.length)
      else
        n
    }).getOrElse("chalice-program")
    val programLocation = program.headOption.map(astNodeToSourceLocation).getOrElse(noLocation)
    val translator = new ProgramTranslator(opts, programName, programLocation)

    translator.onNewMessage += (m => {
      if(m.severity.indicatesFailure) {
        Console.err.println(m)
      } else {
        Console.out.println(m)
      }
    })
    
    translator
  }
  
  def translateToSil(opts : ProgramOptions, program : scala.List[chalice.TopLevelDecl]) : (silAST.programs.Program , Seq[Message])= {
    if(opts.verbose)
      Console.out.println("Beginning translation of Chalice program to SIL.")

    val translator = createTranslator(opts, program)

    translator.translate(program)
  }

  def main(args: Array[String]) {
    val opts = new ProgramOptions()
    val cmdParser = new OptionParser("chalice2sil") {
      // Chalice2SIL options
      opt("v","verbose","Prints additional information about the translation/verification process.",{opts.verbose = true })
      opt("p","print-sil","Prints the translated program in SIL.",{opts.printSil = true})
      opt("f","forward-sil","class name",
        "Forwards the translated SIL program to the `public static main(silAST.Program)` method of the specified class.",
        (c:String) => { opts.forwardSil = Some(c) })
      opt("z3","z3-path","Custom path to Z3.",p => {opts.z3path = Some(p)})

      // Chalice files
      arglistOpt("<chalice-files...>", "The chalice source files.", (source : String) => opts.chaliceFiles.append(source) )
      
      // Options for Chalice
      keyValueOpt("chop","chalice-option","<option>","<value>",
          "Passes an option to Chalice. Can be specified multiple times. A leading dash is added to the chalice option name automatically.", 
          (o:String, v:String) => {opts.chaliceOptions += (o -> v); ()})
          
      // Help
      help("?","help","Displays this help message.")
    }
    
    def convertSlash(s: String): String = 
      if(s.startsWith("/",0)) 
          s.updated(0,'-') 
        else 
          s
    
    if(!cmdParser.parse(args.view.map(convertSlash))){
      // Help has already been printed
      return;
    }

    if(!opts.z3path.isDefined){
      opts.z3path = Some(DefaultConfig.z3path.toAbsolutePath.toString)
    }
    
    val program = invokeChalice(opts) match {
      case None => return;
      case Some(p) => p
    }

    val (silProgram,messages) = translateToSil(opts, program)

    def pluralize(noun : String,  count : Int) = count match {
      case 0 => "no " + noun + "s"
      case 1 => "1 " + noun
      case n => n.toString + " " + noun + "s"
    }

    // Invoke verifier
    // not implemented, for now just print regardless of whether opts.printSil is set
    opts.printSil = true;
    if(opts.printSil){
      Console.out.println(silProgram.toString())
    }

    val warningCount = messages.count(_.severity == Warning)
    val errorCount = messages.count(_.severity.indicatesFailure)
    if(errorCount > 0)
      Console.out.println("[Failure] Chalice2SIL detected %s and %s.".format(
        pluralize("error",errorCount),
        pluralize("warning",warningCount)
      ))
    else
      Console.out.println("[Success] Chalice2SIL detected %s.".format(
        pluralize("warning",warningCount)
      ))

    //Forward SIL to a custom backend
    //TODO: backends might need arguments and might produce messages
    opts.forwardSil match {
      case None =>
        val config = new Config(z3exe = opts.z3path.get)
        val silicon = new Silicon(config)
        val messages = silicon.execute(silProgram)
        for(m <- messages){
          Console.out.println(m.toString) //TODO: unify with chalice2sil message system
        }
      case Some(className) => 
        val classT = java.lang.Class.forName(className)
        val method = classT.getMethod("main",classOf[silAST.programs.Program])
        method.invoke(null,silProgram.asInstanceOf[AnyRef])
    }
  }

}