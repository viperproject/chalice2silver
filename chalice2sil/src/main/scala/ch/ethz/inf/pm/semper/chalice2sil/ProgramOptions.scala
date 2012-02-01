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
  
  var verbose : Boolean = false;
  
  var printSil : Boolean = false;
  
  var forwardSil : Option[String] = None;
  
}