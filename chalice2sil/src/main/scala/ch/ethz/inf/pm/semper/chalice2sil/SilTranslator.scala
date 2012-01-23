package ch.ethz.inf.pm.semper.chalice2sil

import silAST.source.{SourceLocation,noLocation}
import collection.mutable.{ArrayBuffer, Buffer, LinkedHashSet, SynchronizedSet,HashMap}
import silAST.programs.{Program => SilProgram, ProgramFactory}
import silAST.methods.MethodFactory
import silAST.types.referenceType
import silAST.expressions.ExpressionFactory

;

/**
 * Author: Christian Klauser
 */

class SilTranslator(opts : ProgramOptions) {
  /**
   * The SilTranslator will call this function whenever a new message is generated.
   */
  var onNewMessage : Buffer[Message => Unit] = new ArrayBuffer
  private[this] val pastMessages = new LinkedHashSet[Message] with SynchronizedSet[Message]

  /**
   * Reports a message. Will automatically drop duplicate messages.
   */
  protected def report(m : Message){
    if(pastMessages.add(m))
      onNewMessage.foreach(_(m))
  }

  def translate(programName : String,  decls : Seq[chalice.TopLevelDecl]) : (SilProgram,Seq[Message]) = {
    val programLocation =
      decls
        .headOption
        .map(astNodeToSourceLocation)
        .getOrElse(noLocation)

    val pf = SilProgram.getFactory(programLocation,programName)

    decls.foreach(translate(pf,_))
    
    (pf.getProgram,pastMessages.toSeq)
  }

  def dummyExpr(ef:ExpressionFactory, location : SourceLocation):silAST.expressions.Expression = {
    ef.makeEqualityExpression(location, ef.makeIntegerLiteralTerm(location,13),ef.makeIntegerLiteralTerm(location,37))
  }
  
  def translate(ef: ExpressionFactory, e: chalice.Expression)  : silAST.expressions.Expression = {
    e match {
      case otherExpr => 
        report(messages.UnknownAstNode(otherExpr))
        dummyExpr(ef,otherExpr)
    }
  }

  def translate(mf: MethodFactory, method: chalice.Method){
    method.ins.foreach(i => mf.addParameter(i,i.id,referenceType)) // TODO: Translate Chalice.Type to SIL type
    method.outs.foreach(o => mf.addResult(o,o.id,referenceType))
    method.spec.foreach(spec => spec match {
      case chalice.Precondition(e) =>
        val precondition = translate(mf, e)
        mf.addPrecondition(spec,precondition)
      case chalice.Postcondition(e) =>
        val postcondition = translate(mf,e)
        mf.addPostcondition(spec,postcondition)
      case otherSpec => report(messages.UnknownAstNode(otherSpec))
    })
  }

  def translate(pf: ProgramFactory, classNode: chalice.Class){
    classNode.members.foreach({
      case m:chalice.Method  => val mf = pf.getMethodFactory(m,m.id)
                                translate(mf,m)
      case f:chalice.Field => // nothing to do for fields at this time
      case otherNode => report(messages.UnknownAstNode(otherNode))
    })
  }

  def translate(pf : ProgramFactory, decl : chalice.TopLevelDecl) {decl match {
    case c:chalice.Class if c.IsNormalClass => translate(pf,c)
    case c:chalice.Channel => report(messages.ChannelsNotImplemented(c))
    case node => report(messages.UnknownAstNode(node))
  }}
}