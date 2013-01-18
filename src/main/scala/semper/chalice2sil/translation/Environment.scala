package semper.chalice2sil.translation

import semper.sil.ast.expressions.ExpressionFactory
import semper.sil.ast.source.SourceLocation
import math.BigInt._
import semper.chalice2sil.{Message, ProgramOptions}
import collection.mutable.{SynchronizedSet, Buffer}

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

  def dummyExpr(ef:ExpressionFactory, location : SourceLocation):semper.sil.ast.expressions.Expression = {
    ef.makeEqualityExpression(ef.makeIntegerLiteralExpression(13,location),ef.makeIntegerLiteralExpression(37,location),location)
  }

  /**
   * Returns the globally unique name of a method.
   */
  def fullMethodName(m : chalice.Method) : String = m.Parent.id + "::" + m.id

  /**
   * Returns the globally unique name of a field.
   */
  def fullFieldName(f : chalice.Field) = f match {
    case chalice.SpecialField(name,_,_) => name
    case null => throw new IllegalArgumentException("f cannot be null in fullFieldName(f : chalice.Field).")
    case _ => f.Parent.id + "::" + f.id
  }

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

  /**
    * Returns the globally unique name of the predicate representing the monitor invariant.
    * @param c the class for which to provide the monitor invariant name for
    * @return a globally unique name for the monitor invariant of c
    */
  def fullMonitorInvariantName(c : chalice.Class) = c.id + "::$invariant"
}