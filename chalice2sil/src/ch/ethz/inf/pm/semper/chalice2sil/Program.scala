package ch.ethz.inf.pm.semper.chalice2sil

import scopt._

object Program {

  def main(args: Array[String]) {
    val opts = new ProgramOptions()
    val cmdParser = new OptionParser("chalice2sil") {
      // Chalice files
      arglistOpt("<chalice-file>", "The chalice source files.", (source : String) => opts.chaliceFiles.append(source))
      
      // Options for Chalice
      keyValueOpt("chop","chalice-option","<option>","<value>",
          "Passes an option to Chalice. Can be specified multiple times. A leading dash is added to the chalice option name automatically.", 
          (o:String, v:String) => ())
          
      // Help
      help("?","help","Displays this help message.")
    }
    if(cmdParser.parse(args)){
      println("Parse successful");      
    }else{
      // Help has already been printed
      
    }
  }

}