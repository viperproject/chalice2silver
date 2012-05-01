package ch.ethz.inf.pm.semper.chalice2sil

import scala.collection.mutable;
import scala.collection.mutable.HashMap

/**
 * Holds command line options. (Mutable)
 */
class ProgramOptions {
  
  val chaliceOptions : mutable.Map[String, String]  = 
    new HashMap[String,String]()
    
  /**
   * The sequence of source files to be translated.
   */
  val chaliceFiles : mutable.Buffer[String] = 
    new mutable.ArrayBuffer[String]()

  /**
    * Indicates whether to print additional information.
    */
  var verbose : Boolean = false;

  /**
    * Indicates whether the translated SIL program should just be printed instead of verified.
    */
  var printSil : Boolean = false;

  /**
    * The name of a class that the translated program should be forwarded to.
    */
  var forwardSil : Option[String] = None;

  /**
    * A custom path for Z3.
    */
  var z3path : Option[String] = None;
}