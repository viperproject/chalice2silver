package semper.chalice2sil.messages


/**
 * Author: Christian Klauser
 * Date: 23.01.12
 */

/**
 * Classification of the severity of a message.
 * Recommended usage:
 *  import semper.chalice2sil.messages.Severity._
 */
object Severity extends Enumeration {

  /**
   * Classification of the severity of a message.
   */
  type Severity = Value with SeverityValue

  protected class SeverityImpl(name:String, val indicatesFailure : Boolean) extends Val(name) with SeverityValue;
  trait SeverityValue {
    val indicatesFailure : Boolean
  }

  /**
   * Faults indicate that the tool could not perform it's function.
   */
  val Fault = new SeverityImpl("Fault",true) : Severity

  /**
   * An error indicates that there is an issue with the source program or that the verifier returned a negative result.
   */
  val Error = new SeverityImpl("Error",true) : Severity

  /**
   * A warning is issued for situations where the tool can continue working and its result is believed to be correct.
   */
  val Warning = new SeverityImpl("Warning",false) : Severity

  /**
   * An info message is issued to provide insight into the tool's operation.
   */
  val Info = new SeverityImpl("Info",false) : Severity
}