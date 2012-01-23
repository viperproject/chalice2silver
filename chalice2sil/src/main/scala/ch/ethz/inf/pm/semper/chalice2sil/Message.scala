package ch.ethz.inf.pm.semper.chalice2sil

import messages.{Severity, MessageId}
import silAST.source.SourceLocation
import collection.mutable.{SynchronizedMap, HashMap}

/**
 * Author: Christian Klauser
 * Date: 23.01.12
 */

abstract class Message(val id : MessageId, val location : SourceLocation, severityOverride : Option[Severity] = None){
  def severity = severityOverride.getOrElse(id.defaultSeverity)
  def data : Iterable[Any]
  private def removeEmptyParens(s:String) = if (s.endsWith("()")) s.dropRight(2) else s
  override def toString =
    "[%s] %s: %s".format(removeEmptyParens(severity.toString), location, id.messageFormat.format(data.toSeq : _*))
  override def equals(arg : Any) = arg match {
    case m:Message => id == m.id && data == m.data
    case _ => false
  }
  override def hashCode = (id##) ^ (data##)
}
