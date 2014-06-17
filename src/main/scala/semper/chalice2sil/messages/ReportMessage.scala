package semper.chalice2sil.messages

import semper.sil.ast._
import semper.chalice2sil.TranslationError

class ReportMessage(val message: String, val position: Position, val fatal: Boolean = false) {
  val translationError = if (fatal) TranslationError(message, position) else null
  override def toString = (if(position!=null) position.toString + ": " else "") + message
}

class DeprecatedFeature(m: String, p: Position) extends ReportMessage("Deprecated Feature: " + m, p, true)

class UnsupportedFeature(m: String, p: Position, f: Boolean = false)
  extends ReportMessage("Unsupported Feature: " + m, p, f)

case class Channels(p: Position = null)
  extends UnsupportedFeature("Channels." + (if(p==null) "" else " The code may be translated incorrectly."), p, true)
  // todo

case class DeadlockAvoidance(p: Position)
  extends UnsupportedFeature("Deadlock Avoidance. The feature is presently ignored.", p) // todo

case class TokenPermissions(p: Position)
  extends UnsupportedFeature("Token Permission. The code may be translated incorrectly.", p, true) // todo

case class Backpointers(p: Position)
  extends UnsupportedFeature("Backpointers are under construction. The code may be translated incorrectly.", p, true)
    // todo: need to fix object construction and field updates only

case class Aggregates(p: Position)
  extends UnsupportedFeature("Aggregates. The code may be translated incorrectly.", p, true)

case class Signals(p: Position)
  extends UnsupportedFeature("Signals. The code may be translated incorrectly.", p, true)

case class SpecStatement(p: Position)
  extends DeprecatedFeature("Specification Statement. The code may be translated incorrectly.", p)

case class UnsupportedStatement(val statement: String, p: Position)
  extends DeprecatedFeature("Statement " + statement + ". The code may be translated incorrectly.", p)

case class Eval(p: Position)
  extends DeprecatedFeature("eval. The code may be translated incorrectly.", p)

case class Strings(p: Position)
  extends UnsupportedFeature("Strings. The code may be translated incorrectly.", p, true) // todo

case class OldLockModel(p: Position = null)
  extends DeprecatedFeature("Old Lock Model. The code may be translated incorrectly.", p)

case class ReadLocks(val p: Position)
  extends DeprecatedFeature("Read Locks. The code may be translated incorrectly.", p)

case class MuReordering(p: Position)
  extends DeprecatedFeature("Mu Reordering. The code may be translated incorrectly.", p)