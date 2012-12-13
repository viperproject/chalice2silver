package ch.ethz.inf.pm.semper.chalice2sil.translation.util

/**
 * @author Christian Klauser
 */
class MangleException(message : String, val originalText : String) extends RuntimeException(message) {

}
