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
   * Returns the globally unique name of a method.
   */
  def fullMethodName(m : chalice.Method) = m.Parent.id + "::" + m.id

  /**
   * Returns the globally unique name of a field.
   */
  def fullFieldName(f : chalice.Field) = f.Parent.id + "::" + f.id

  /**
    * Returns the globally unique name of a predicate.
    * @param p The predicate to provide a name for
    * @return a globally unique name for p.
    */
  def fullPredicateName(p : chalice.Predicate) = p.Parent.id + "::" + p.Id

  /**
    * Returns the globally unique name of a function.
    * @param f the function to provide a name for
    * @return a globally unique name for f
    */
  def fullFunctionName(f : chalice.Function) = f.Parent.id + "::" + f.Id

}