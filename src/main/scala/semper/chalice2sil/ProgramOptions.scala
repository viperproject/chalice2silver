package semper.chalice2sil

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
  var chaliceFile : String = null
}