package semper.chalice2sil

import messages.MessageId
import messages.Severity._
import semper.sil.ast.source.SourceLocation

abstract class Message(val id : MessageId, val location : SourceLocation, severityOverride : Option[Severity] = None){
  def severity = severityOverride.getOrElse(id.defaultSeverity)
  def data : Iterable[Any]
  override def toString =
    "[%s] %s: %s".format(severity, location, id.messageFormat.format(data.toSeq : _*))
  override def equals(arg : Any) = arg match {
    case m:Message => id == m.id && data == m.data
    case _ => false
  }
  override def hashCode = (id##) ^ (data##)
}
