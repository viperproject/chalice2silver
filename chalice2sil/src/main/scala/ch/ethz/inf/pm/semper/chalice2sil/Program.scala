package ch.ethz.inf.pm.semper.chalice2sil

import messages.{Warning, Fault, Error}
import scopt._
import chalice.{Chalice,PrintProgram}

object Program {

  def main(args: Array[String]) {
    println("Arguments")
    args.foreach((s) => {print(" "); print(s);}) 
    println
    
    val opts = new ProgramOptions()
    val cmdParser = new OptionParser("chalice2sil") {
      // Chalice2SIL options
      opt("verbose","v","Prints additional information about the translation/verification process.",{opts.verbose = true })
      opt("print-sil","p","Prints the translated program in SIL.",{opts.printSil = true})
      
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
    }// */
    
    val chOptsBuilder = scala.collection.mutable.ArrayBuilder.make[String]()
    opts.chaliceOptions.foreach((entry) => {
      val (option,value) = entry
      if(value.isEmpty())
          chOptsBuilder += "/" + option
        else
          chOptsBuilder += "/" + option + ":"  + value
    })
    chOptsBuilder ++= opts.chaliceFiles    
    val chOpts = chOptsBuilder.result()
    
    println("Forwarding to Chalice (" + chOpts.length + ")")
    chOpts.foreach((s) => {print(" "); print(s)})
    println();
    
    val chaliceParams = Chalice.parseCommandLine(chOpts) match {
      case Some(p) => p
      case None =>
        if (opts.verbose)
          Console.out.println("Chalice failed to parse command line options. Chalice2SIL terminates.");
        return;
    }

    val program = Chalice.parsePrograms(chaliceParams) match {
      case Some(p) => p
      case None =>
        if (opts.verbose)
          Console.out.println("Chalice program contained syntax errors. Chalice2SIL terminates.");
        return; //illegal program, errors have already been displayed
    }
    
    if(!chaliceParams.doTypecheck || !Chalice.typecheckProgram(chaliceParams, program)){
      if (opts.verbose)
        Console.out.println("Chalice program contained type errors. Chalice2SIL terminates.")
      return;
    }
    
    if (chaliceParams.printProgram) {
      Console.out.println("Program after type checking: ");
      PrintProgram.P(program)
    }
    
    if(!chaliceParams.doTranslate) {
      return;
    }

    if (opts.verbose)
      Console.out.println("Beginning translation of Chalice program to SIL.")
    
    // Translate to SIL
    val translator = new SilTranslator(opts)
    translator.onNewMessage += (m => {
      if (m.severity.indicatesFailure){
        Console.err.println(m)
      } else {
        Console.out.println(m)
      }
    })
    
    val (silProgram,messages) = translator.translate(opts.chaliceFiles.headOption.getOrElse("chalice_program"), program)

    def pluralize(noun : String,  count : Int) = count match {
      case 0 => "no " + noun + "s"
      case 1 => "1 " + noun
      case n => n.toString + " " + noun + "s"
    }

    // Invoke verifier
    // not implemented, for now just print regardless of whether opts.printSil is set
    opts.printSil = true;
    if(opts.printSil){
      Console.out.println(silProgram.toString)
    }

    val warningCount = messages.count(_.severity == Warning())
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
  }

}