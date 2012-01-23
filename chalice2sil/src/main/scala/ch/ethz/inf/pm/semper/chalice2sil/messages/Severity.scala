package ch.ethz.inf.pm.semper.chalice2sil.messages

/**
 * Author: Christian Klauser
 * Date: 23.01.12
 */

/**
 * Classification of the severity of a message.
 */
sealed abstract class Severity {
  val indicatesFailure = false
}

/**
 * Faults indicate that the tool could not perform it's function.
 */
case class Fault() extends Severity { override val indicatesFailure = true }

/**
 * An error indicates that there is an issue with the source program or that the verifier returned a negative result.
 */
case class Error() extends Severity { override val indicatesFailure = true }

/**
 * A warning is issued for situations where the tool can continue working and its result is believed to be correct.
 */
case class Warning() extends Severity;

/**
 * An info message is issued to provide insight into the tool's operation.
 */
case class Info() extends Severity;