package semper.chalice2sil

import scala.language.postfixOps
import messages.MessageId
import messages.Severity._
import semper.sil.ast.Position

abstract class Message(val id : MessageId, n: chalice.ASTNode, severityOverride : Option[Severity] = None){
  val position = MessageId.getPos(n)
  def severity = severityOverride.getOrElse(id.defaultSeverity)
  def data : Iterable[Any]
  override def toString =
    "[%s] %s: %s".format(severity, position, id.messageFormat.format(data.toSeq : _*))
  override def equals(arg : Any) = arg match {
    case m:Message => id == m.id && data == m.data
    case _ => false
  }
  override def hashCode = (id##) ^ (data##)
}
