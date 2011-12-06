package ch.ethz.inf.pm.semper.chalice2sil

import scopt._
import chalice.{Chalice,PrintProgram}

object Program {

  def main(args: Array[String]) {
    println("Arguments")
    args.foreach((s) => {print(" "); print(s);}) 
    println
    
    val opts = new ProgramOptions()
    val cmdParser = new OptionParser("chalice2sil") {
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
    
    val params = Chalice.parseCommandLine(chOpts) match {
      case Some(p) => p
      case None => return;
    }

    val program = Chalice.parsePrograms(params) match {
      case Some(p) => p
      case None => return //illegal program, errors have already been displayed
    }
    
    if(!params.doTypecheck || !Chalice.typecheckProgram(params, program))
      return;
    
    if (params.printProgram) {
      Console.out.println("Program after type checking: ");
      PrintProgram.P(program)
    }
    
    if(!params.doTranslate)
      return;
    
    // Translate to SIL
    
    // Invoke verifier
  }

}