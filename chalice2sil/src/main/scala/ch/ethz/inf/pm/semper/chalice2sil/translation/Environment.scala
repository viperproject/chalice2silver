package ch.ethz.inf.pm.semper.chalice2sil.translation

import silAST.expressions.ExpressionFactory
import silAST.source.SourceLocation
import math.BigInt._
import ch.ethz.inf.pm.semper.chalice2sil.{Message, ProgramOptions}
import collection.mutable.{SynchronizedSet, LinkedHashSet, Buffer}

trait Environment {
  def programOptions : ProgramOptions
  def onNewMessage : Buffer[Message => Unit]
  def pastMessages : scala.collection.mutable.Set[Message] with SynchronizedSet[Message]

  /**
   * Reports a message. Will automatically drop duplicate messages.
   */
  protected def report(m : Message){
    if(pastMessages.add(m))
      onNewMessage.foreach(_(m))
  }

  def dummyExpr(ef:ExpressionFactory, location : SourceLocation):silAST.expressions.Expression = {
    ef.makeEqualityExpression(location, ef.makeIntegerLiteralTerm(location,13),ef.makeIntegerLiteralTerm(location,37))
  }

  /**
   * Returns a globally unique name of a method.
   */
  def fullMethodName(m : chalice.Method) = m.Parent.id + "::" + m.id

  /**
   * Returns a globally unique name of a field.
   */
  def fullFieldName(f : chalice.Field) = f.Parent.id + "::" + f.id


}